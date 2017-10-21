{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LLVMFrontend.Helpers where
  import Data.ByteString.Short
  import Control.Monad.State

  import LLVM.AST
  import LLVM.AST.Type
  import LLVM.AST.Global
  import qualified LLVM.AST as AST

  import qualified LLVM.AST.Linkage as LL
  import qualified LLVM.AST.Attribute as LA
  import qualified LLVM.AST.CallingConvention as LCC
  import qualified LLVM.AST.FloatingPointPredicate as LFP
  import qualified LLVM.AST.Instruction as LI
  import qualified LLVM.AST.Global as LG
  import qualified LLVM.AST.Constant as LC
  import qualified LLVM.AST.Operand as LO
  import qualified LLVM.AST.Name as LN
  import qualified LLVM.AST.Type as LT
  import qualified LLVM.AST.Float as LF

  -- Used in generation of LLVM backend code
  newtype LLVM a = LLVM (State AST.Module a)
    deriving (Functor, Applicative, Monad, MonadState AST.Module)

  -- Creates a simple function which returns void and takes no arguments
  defineFn :: LT.Type -> ShortByteString -> [BasicBlock] -> Definition
  defineFn typ label body =
    GlobalDefinition $ functionDefaults {
      name = Name label,
      returnType = typ,
      basicBlocks = body
    }


  -- Create a reference to a local variable
  local ::  Type -> Name -> Operand
  local = LocalReference

  -- Create a reference to a global variable
  global :: Type -> Name -> LC.Constant
  global = LC.GlobalReference

  -- Create a reference to an 'extern'd variable
  externf :: Type -> Name -> Operand
  externf ty nm = ConstantOperand (LC.GlobalReference ty nm)

  {- Constants -}

  -- 32-bit integer constant
  int :: Integral a => a -> LC.Constant
  int = LC.Int 32 . fromIntegral

  float :: Float -> LC.Constant
  float = LC.Float . LF.Single

  add :: Operand -> Operand -> Instruction
  add a b = LI.Add False False a b []

  -- Arithmetic and Constants for floating points
  fadd :: Operand -> Operand -> Instruction
  fadd a b = FAdd NoFastMathFlags a b []

  fsub :: Operand -> Operand -> Instruction
  fsub a b = FSub NoFastMathFlags a b []

  fmul :: Operand -> Operand -> Instruction
  fmul a b = FMul NoFastMathFlags a b []

  fdiv :: Operand -> Operand -> Instruction
  fdiv a b = FDiv NoFastMathFlags a b []

  fcmp :: LFP.FloatingPointPredicate -> Operand -> Operand -> Instruction
  fcmp cond a b = FCmp cond a b []

  cons :: LC.Constant -> Operand
  cons = ConstantOperand

  uitofp :: Type -> Operand -> Instruction
  uitofp ty a = UIToFP a ty []

  toArgs :: [Operand] -> [(Operand, [LA.ParameterAttribute])]
  toArgs = map (\x -> (x, []))

  -- Effects
  call :: Operand -> [Operand] -> Instruction
  call fn args =  Call Nothing LCC.C [] (Right fn) (toArgs args) [] []

  alloca :: Type -> Instruction
  alloca ty = Alloca ty Nothing 0 []

  store :: Operand -> Operand -> Instruction
  store ptr val = Store False ptr val Nothing 0 []

  load :: Operand -> Instruction
  load ptr =  Load False ptr Nothing 0 []

  -- Control Flow
  br :: Name -> Named Terminator
  br val = Do $ Br val []

  cbr :: Operand -> Name -> Name -> Named Terminator
  cbr cond tr fl = Do $ CondBr cond tr fl []

  phi :: Type -> [(Operand, Name)] -> Instruction
  phi ty incoming = Phi ty incoming []

  ret :: Operand -> Named Terminator
  ret val = Do $ Ret (Just val) []

  retvoid :: Named Terminator
  retvoid = Do $ Ret Nothing []
