-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module LLVMFrontend.CFG where
  import Control.Monad.State
  import Control.Lens
  import Control.Monad.Extra

  import qualified Data.Vector as Vec
  import Data.Vector(Vector)
  import qualified Data.ByteString.Lazy as B

  import JVM.ClassFile
  import qualified JVM.Assembler as J
  import qualified LLVM.AST.Instruction as LI
  import qualified LLVM.AST.Global as LG
  import qualified LLVM.AST.Constant as LC
  import qualified LLVM.AST.Operand as LO
  import qualified LLVM.AST.Name as LN
  import qualified LLVM.AST.Type as LT
  import LLVMFrontend.Helpers

  -- Control-Flow Graph state
  data CFGState = CFGState {
    basicBlocks :: [LG.BasicBlock],
    programCounter :: Int,
    instructions :: Vector J.Instruction,
    codeMetaData :: J.Code,
    operandStack :: [LO.Operand],
    localVariableNames :: [LN.Name],
    -- Monotonic counter for unnamed temporaries
    autotmpN :: Int
  } deriving (Show)
  -- makeLenses ''CFGState

  type CFG = StateT CFGState IO

  -- Parse a JVM method into an LLVM module
  parseCFG :: J.Code -> IO CFGState
  parseCFG code = execStateT parseInstructions defaultCFGState
    where
      defaultCFGState = CFGState {
        basicBlocks = [LG.BasicBlock (LN.Name "entry") [] retvoid],
        programCounter = 0,
        instructions = Vec.fromList (J.codeInstructions code),
        codeMetaData = code,
        autotmpN = 2,
        operandStack = [],
        localVariableNames = []
      }

  -- Get next numeric identifier for a new temporary
  nextTemp :: Integral a => CFG a
  nextTemp = do
    curr <- gets autotmpN
    modify $ \s -> s { autotmpN = curr + 1 }
    return . fromIntegral $ curr

  -- Pushes a constant on the operand stack.
  pushConstant :: LC.Constant -> CFG ()
  pushConstant cons =
    -- An LLVM constant must be first converted into a ConstantOperand
    pushOperand $ LO.ConstantOperand cons

  -- Pushes an operand on the operand stack.
  pushOperand :: LO.Operand -> CFG ()
  pushOperand op = do
    ops <- gets operandStack
    modify $ \s -> s { operandStack = op : ops }

  -- Pops a constant off the operand stack...
  -- Note that the caller must know the type of the actual operand
  popOperand :: CFG LO.Operand
  popOperand = do
    ops <- gets operandStack
    let op = head ops
    modify $ \s -> s { operandStack = tail ops }
    return op

  updateCurrentBlock :: LG.BasicBlock -> CFG ()
  updateCurrentBlock b = do
    blocks <- gets basicBlocks
    modify $ \s -> s { basicBlocks = b : tail blocks }

  -- Sets the current terminator of this basic block. The terminator is an terminal
  -- operation (I.E one that is the very last instruction to be called) and generally
  -- results in branching of control flow or a return statement
  setTerminator :: LI.Named LI.Terminator -> CFG ()
  setTerminator term = do
    block <- head <$> gets basicBlocks
    updateCurrentBlock $ setTerminator' term block

    where
      setTerminator' term (LG.BasicBlock n i t) = LG.BasicBlock n i term


  -- Appends an instruction to the specified basic block by creating a copy containing
  -- the requested instruction.
  appendInstruction :: LI.Named LI.Instruction -> CFG ()
  appendInstruction instr = do
    block <- head <$> gets basicBlocks
    updateCurrentBlock $ appendInstruction' instr block

    where
      appendInstruction' i (LG.BasicBlock n is t) = LG.BasicBlock n (is ++ [i]) t


  -- Generate a named local variable name for each possible local variable
  setupLocals :: CFG ()
  setupLocals = do
    -- For each maximum locals, generate a name for it in advance
    maxLocals <- J.codeMaxLocals <$> gets codeMetaData
    names <- forM [0..maxLocals] $ \i -> do
      let name = LN.mkName ("L" ++ show i)
      appendInstruction $ name LI.:= alloca LT.i32
      return name
    modify $ \s -> s { localVariableNames = names }

  -- Obtains the local variable at requested index
  getLocal :: Integral a => a -> CFG LN.Name
  getLocal idx = (!! fromIntegral idx) <$> gets localVariableNames

  -- Parse JVM Bytecode instructions into LLVM IR instructions
  parseInstructions :: CFG ()
  parseInstructions = do
    -- Fetch and Add to program counter.
    pc <- gets programCounter
    modify $ \s -> s { programCounter = pc + 1 }

    -- Decode instructions.
    -- TODO: In future, check type of instruction and go from there...
    instr <- (Vec.! pc) <$> gets instructions
    case instr of
      -- Constants are pushed directly on operand stack
      J.ICONST_0 -> pushConstant $ int 0
      J.ICONST_1 -> pushConstant $ int 1
      J.ICONST_2 -> pushConstant $ int 2
      J.ICONST_3 -> pushConstant $ int 3
      J.ICONST_4 -> pushConstant $ int 4
      J.ICONST_5 -> pushConstant $ int 5
      J.ICONST_M1 -> pushConstant $ int (-1)

      -- Loads/Stores are assigned directly via Alloca
      -- TODO: Need SSA conversions
      J.ISTORE_ idx -> do
        -- The index of the local variable normally is the next byte, but the
        -- 'hs-java' library is gracious enough to provide us with a helper that
        -- tags the next byte along with it.
        localName <- getLocal $
          case idx of
            J.I0 -> 0
            J.I1 -> 1
            J.I2 -> 2
            J.I3 -> 3

        storeInstr <- store (local LT.i32 localName) <$> popOperand
        appendInstruction (LI.Do storeInstr)

      -- Binary operations operate and utilize the operand stack to simulate the
      -- stack-machine nature of the JVM on the LLVM.
      J.IADD -> do
        -- Map a JVM-Bytecode addition instruction to an LLVM addition instruction
        llvmInstr <- add <$> popOperand <*> popOperand
        tmpName <- LN.UnName <$> nextTemp
        pushOperand $ LO.LocalReference LT.i32 tmpName
        appendInstruction $ tmpName LI.:= llvmInstr
      -- Return instructions will pop off the top of the operand stack and use it as
      -- the return value.
      J.IRETURN -> do
        retInstr <- ret <$> popOperand
        setTerminator retInstr
      _ -> return ()

    whenM outOfInstructions $ do
      retInstr <- ret <$> popOperand
      setTerminator retInstr
    unlessM outOfInstructions parseInstructions

    where
      outOfInstructions :: CFG Bool
      outOfInstructions = do
        pc <- gets programCounter
        pcMax <- Vec.length <$> gets instructions
        return $ pc == pcMax
