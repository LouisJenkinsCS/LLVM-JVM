-- {-# LANGUAGE TemplateHaskell #-}
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

  -- Control-Flow Graph state
  data CFGState = CFGState {
    basicBlocks :: [LG.BasicBlock],
    programCounter :: Int,
    instructions :: Vector J.Instruction,
    codeMetaData :: J.Code
  } deriving (Show)
  -- makeLenses ''CFGState

  type CFG = StateT CFGState IO

  parseCFG :: J.Code -> IO CFGState
  parseCFG code = execStateT parseInstructions defaultCFGState
    where
      defaultCFGState = CFGState {
        basicBlocks = [],
        programCounter = 0,
        instructions = Vec.fromList (J.codeInstructions code),
        codeMetaData = code
      }

  parseInstructions :: CFG ()
  parseInstructions = do
    -- Fetch and Add to program counter.
    pc <- gets programCounter
    modify $ \s -> s { programCounter = pc + 1 }

    -- Decode instructions.
    -- TODO: In future, check type of instruction and go from there...
    instrs <- (: []) . (Vec.! pc) <$> gets instructions

    -- Append to our current block...
    block <- head <$> gets basicBlocks

    unlessM outOfInstructions parseInstructions

    where
      outOfInstructions :: CFG Bool
      outOfInstructions = do
        pc <- gets programCounter
        pcMax <- Vec.length <$> gets instructions
        return $ pc == pcMax
