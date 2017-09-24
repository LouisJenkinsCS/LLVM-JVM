{-
  CodeGen example ripped from Haskell Kaleidoscope LLVM tutorial for a decent headstart...
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

  import qualified Data.ByteString.Char8 as C8
  import qualified Data.ByteString.Short as BS

  import Data.List
  import Data.Function
  import qualified Data.Map as Map

  import Control.Monad.State

  import LLVM.AST
  import LLVM.AST.Global
  import qualified LLVM.AST as AST

  import qualified LLVM.AST.Linkage as L
  import qualified LLVM.AST.Constant as C
  import qualified LLVM.AST.Attribute as A
  import qualified LLVM.AST.CallingConvention as CC
  import qualified LLVM.AST.FloatingPointPredicate as FP
  import LLVM.AST.Type

  -------------------------------------------------------------------------------
  -- Module Level
  -------------------------------------------------------------------------------

  newtype LLVM a = LLVM (State AST.Module a)
    deriving (Functor, Applicative, Monad, MonadState AST.Module )

  runLLVM :: AST.Module -> LLVM a -> AST.Module
  runLLVM mod (LLVM m) = execState m mod

  emptyModule :: String -> AST.Module
  emptyModule label = defaultModule { moduleName = BS.toShort . C8.pack $ label }

  addDefn :: Definition -> LLVM ()
  addDefn d = do
    defs <- gets moduleDefinitions
    modify $ \s -> s { moduleDefinitions = defs ++ [d] }

  define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
  define retty label argtys body = addDefn $
    GlobalDefinition $ functionDefaults {
      name        = Name . BS.toShort . C8.pack $ label
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = body
    }

  external ::  Type -> String -> [(Type, Name)] -> LLVM ()
  external retty label argtys = addDefn $
    GlobalDefinition $ functionDefaults {
      -- TODO: Fix this so it does not need to do 2 * O(N) operations... Same for others
      name        = Name . BS.toShort . C8.pack $ label
    , linkage     = L.External
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = []
    }


  -------------------------------------------------------------------------------
  -- Names
  -------------------------------------------------------------------------------

  type Names = Map.Map String Int

  uniqueName :: String -> Names -> (String, Names)
  uniqueName nm ns =
    case Map.lookup nm ns of
      Nothing -> (nm,  Map.insert nm 1 ns)
      Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

  -------------------------------------------------------------------------------
  -- Codegen State
  -------------------------------------------------------------------------------

  type SymbolTable = [(String, Operand)]

  data CodegenState
    = CodegenState {
      currentBlock :: Name                     -- Name of the active block to append to
    , blocks       :: Map.Map Name BlockState  -- Blocks for function
    , symtab       :: SymbolTable              -- Function scope symbol table
    , blockCount   :: Int                      -- Count of basic blocks
    , count        :: Word                     -- Count of unnamed instructions
    , names        :: Names                    -- Name Supply
    } deriving Show

  data BlockState
    = BlockState {
      idx   :: Int                            -- Block index
    , stack :: [Named Instruction]            -- Stack of instructions
    , term  :: Maybe (Named Terminator)       -- Block terminator
    } deriving Show

  -------------------------------------------------------------------------------
  -- Codegen Operations
  -------------------------------------------------------------------------------

  newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState )

  sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
  sortBlocks = sortBy (compare `on` (idx . snd))

  createBlocks :: CodegenState -> [BasicBlock]
  createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

  makeBlock :: (Name, BlockState) -> BasicBlock
  makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
    where
      maketerm (Just x) = x
      maketerm Nothing = error $ "Block has no terminator: " ++ show l

  entryBlockName :: String
  entryBlockName = "entry"

  emptyBlock :: Int -> BlockState
  emptyBlock i = BlockState i [] Nothing

  emptyCodegen :: CodegenState
  emptyCodegen = CodegenState (Name . BS.toShort . C8.pack $ entryBlockName) Map.empty [] 1 0 Map.empty

  execCodegen :: Codegen a -> CodegenState
  execCodegen m = execState (runCodegen m) emptyCodegen

  fresh :: Codegen Word
  fresh = do
    i <- gets count
    modify $ \s -> s { count = 1 + i }
    return $ i + 1

  instr :: Instruction -> Codegen Operand
  instr ins = do
    n <- fresh
    let ref = UnName n
    blk <- current
    let i = stack blk
    modifyBlock (blk { stack = (ref := ins) : i } )
    return $ local ref

  terminator :: Named Terminator -> Codegen (Named Terminator)
  terminator trm = do
    blk <- current
    modifyBlock (blk { term = Just trm })
    return trm

  -------------------------------------------------------------------------------
  -- Block Stack
  -------------------------------------------------------------------------------

  entry :: Codegen Name
  entry = gets currentBlock

  addBlock :: String -> Codegen Name
  addBlock bname = do
    bls <- gets blocks
    ix  <- gets blockCount
    nms <- gets names

    let new = emptyBlock ix
        (qname, supply) = uniqueName bname nms

    modify $ \s -> s { blocks = Map.insert (Name . BS.toShort . C8.pack $ qname) new bls
                     , blockCount = ix + 1
                     , names = supply
                     }
    return (Name . BS.toShort . C8.pack $ qname)

  setBlock :: Name -> Codegen Name
  setBlock bname = do
    modify $ \s -> s { currentBlock = bname }
    return bname

  getBlock :: Codegen Name
  getBlock = gets currentBlock

  modifyBlock :: BlockState -> Codegen ()
  modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { blocks = Map.insert active new (blocks s) }

  current :: Codegen BlockState
  current = do
    c <- gets currentBlock
    blks <- gets blocks
    case Map.lookup c blks of
      Just x -> return x
      Nothing -> error $ "No such block: " ++ show c

  -------------------------------------------------------------------------------
  -- Symbol Table
  -------------------------------------------------------------------------------

  assign :: String -> Operand -> Codegen ()
  assign var x = do
    lcls <- gets symtab
    modify $ \s -> s { symtab = (var, x) : lcls }

  getvar :: String -> Codegen Operand
  getvar var = do
    syms <- gets symtab
    case lookup var syms of
      Just x  -> return x
      Nothing -> error $ "Local variable not in scope: " ++ show var

  -------------------------------------------------------------------------------

  -- References
  local ::  Name -> Operand
  local = LocalReference i64

  global ::  Name -> C.Constant
  global = C.GlobalReference i64

  externf :: Name -> Operand
  externf = ConstantOperand . C.GlobalReference i64

  -- Arithmetic and Constants
  fadd :: Operand -> Operand -> Codegen Operand
  fadd a b = instr $ FAdd NoFastMathFlags a b []

  fsub :: Operand -> Operand -> Codegen Operand
  fsub a b = instr $ FSub NoFastMathFlags a b []

  fmul :: Operand -> Operand -> Codegen Operand
  fmul a b = instr $ FMul NoFastMathFlags a b []

  fdiv :: Operand -> Operand -> Codegen Operand
  fdiv a b = instr $ FDiv NoFastMathFlags a b []

  fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
  fcmp cond a b = instr $ FCmp cond a b []

  cons :: C.Constant -> Operand
  cons = ConstantOperand

  uitofp :: Type -> Operand -> Codegen Operand
  uitofp ty a = instr $ UIToFP a ty []

  toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
  toArgs = map (\x -> (x, []))

  -- Effects
  call :: Operand -> [Operand] -> Codegen Operand
  call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

  alloca :: Type -> Codegen Operand
  alloca ty = instr $ Alloca ty Nothing 0 []

  store :: Operand -> Operand -> Codegen Operand
  store ptr val = instr $ Store False ptr val Nothing 0 []

  load :: Operand -> Codegen Operand
  load ptr = instr $ Load False ptr Nothing 0 []

  -- Control Flow
  br :: Name -> Codegen (Named Terminator)
  br val = terminator $ Do $ Br val []

  cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
  cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

  ret :: Operand -> Codegen (Named Terminator)
  ret val = terminator $ Do $ Ret (Just val) []
