{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LLVMFrontend.Helpers where
  import Data.ByteString.Short
  import Control.Monad.State

  import LLVM.AST
  import LLVM.AST.Type
  import LLVM.AST.Global
  import qualified LLVM.AST as AST

  import qualified LLVM.AST.Linkage as L
  import qualified LLVM.AST.Constant as C
  import qualified LLVM.AST.Attribute as A
  import qualified LLVM.AST.CallingConvention as CC
  import qualified LLVM.AST.FloatingPointPredicate as FP

  -- Used in generation of LLVM backend code
  newtype LLVM a = LLVM (State AST.Module a)
    deriving (Functor, Applicative, Monad, MonadState AST.Module)

  -- Creates a simple function which returns void and takes no arguments
  defineFn :: ShortByteString -> [BasicBlock] -> Definition
  defineFn label body =
    GlobalDefinition $ functionDefaults {
      name = Name label,
      returnType = VoidType,
      basicBlocks = body
    }


  -- Create a reference to a local variable
  local ::  Type -> Name -> Operand
  local = LocalReference

  -- Create a reference to a global variable
  global :: Type -> Name -> C.Constant
  global = C.GlobalReference

  -- Create a reference to an 'extern'd variable
  externf :: Type -> Name -> Operand
  externf ty nm = ConstantOperand (C.GlobalReference ty nm)

  -- Arithmetic and Constants for floating points
  fadd :: Operand -> Operand -> Instruction
  fadd a b = FAdd NoFastMathFlags a b []

  fsub :: Operand -> Operand -> Instruction
  fsub a b = FSub NoFastMathFlags a b []

  fmul :: Operand -> Operand -> Instruction
  fmul a b = FMul NoFastMathFlags a b []

  fdiv :: Operand -> Operand -> Instruction
  fdiv a b = FDiv NoFastMathFlags a b []

  fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Instruction
  fcmp cond a b = FCmp cond a b []

  cons :: C.Constant -> Operand
  cons = ConstantOperand

  uitofp :: Type -> Operand -> Instruction
  uitofp ty a = UIToFP a ty []

  toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
  toArgs = map (\x -> (x, []))

  -- Effects
  call :: Operand -> [Operand] -> Instruction
  call fn args =  Call Nothing CC.C [] (Right fn) (toArgs args) [] []

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
