module VirtualMachine.Types where
  import Data.Word
  import Data.IORef
  import Data.Bits
  import Data.Map
  import Data.Array.IO
  import ClassFile.Types
  import Control.Monad
  import Text.Printf
  import Data.STRef
  import Numeric

  type Stack = IORef [StackFrame]

  type StackFrame = IORef Stack_Frame

  type ByteCode = Word8
  type Instructions = [ByteCode]



  data Code_Segment = Code {
    byte_code :: Instructions,
    program_counter :: IORef Word32
  }

  -- Temporary type until a proper heap is setup
  type Object = Int

  liftVInt :: (Int -> Int) -> Value -> Value
  liftVInt f (VInt x) = VInt (f x)
  liftVInt2 :: (Int -> Int -> Int) -> Value -> Value -> Value
  liftVInt2 f (VInt x) (VInt y) = VInt (f x y)

  liftVLong :: (Integer -> Integer) -> Value -> Value
  liftVLong f (VLong x) = VLong (f x)
  liftVLong2 :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
  liftVLong2 f (VLong x) (VLong y) = VLong (f x y)

  liftVFloat :: (Float -> Float) -> Value -> Value
  liftVFloat f (VFloat x) = VFloat (f x)
  liftVFloat2 :: (Float -> Float -> Float) -> Value -> Value -> Value
  liftVFloat2 f (VFloat x) (VFloat y) = VFloat (f x y)

  liftVDouble :: (Double -> Double) -> Value -> Value
  liftVDouble f (VDouble x) = VDouble (f x)
  liftVDouble2 :: (Double -> Double -> Double) -> Value -> Value -> Value
  liftVDouble2 f (VDouble x) (VDouble y) = VDouble (f x y)

  -- Wrap Java native primivitve types in Haskell types
  data Value = VInt Int | VLong Integer| VFloat Float | VDouble Double | VReference Object
    deriving (Eq, Ord)

  instance Num Value where
    (+) (VInt x) (VInt y) = VInt (x + y)
    (+) (VLong x) (VLong y) = VLong (x + y)
    (+) (VFloat x) (VFloat y) = VFloat (x + y)
    (+) (VDouble x) (VDouble y) = VDouble (x + y)
    (+) _ _ = error "Bad Op: Addition"

    (-) (VInt x) (VInt y) = VInt (x - y)
    (-) (VLong x) (VLong y) = VLong (x - y)
    (-) (VFloat x) (VFloat y) = VFloat (x - y)
    (-) (VDouble x) (VDouble y) = VDouble (x - y)
    (-) _ _ = error "Bad Op: Subtraction"

    (*) (VInt x) (VInt y) = VInt (x * y)
    (*) (VLong x) (VLong y) = VLong (x * y)
    (*) (VFloat x) (VFloat y) = VFloat (x * y)
    (*) (VDouble x) (VDouble y) = VDouble (x * y)
    (*) _ _ = error "Bad Op: Multiplication"

  instance Real Value where
    toRational (VInt x) = toRational x
    toRational (VLong x) = toRational x
    toRational (VFloat x) = toRational x
    toRational (VDouble x) = toRational x
    toRational _ = error "Bad Op: toRational"

  instance Enum Value where
    toEnum _ = error "Bad Op: toEnum"
    fromEnum _ = error "Bad Op: fromEnum"

  instance Integral Value where
    div (VInt x) (VInt y) = VInt (x `div` y)
    div (VLong x) (VLong y) = VLong (x `div` y)
    div (VFloat x) (VFloat y) = VFloat (x / y)
    div (VDouble x) (VDouble y) = VDouble (x / y)
    div _ _ = error "Bad Op: div"

    toInteger (VInt x) = toInteger x
    toInteger (VLong x) = toInteger x
    toInteger _ = error "Bad Op: toInteger"

  instance Show Value where
    show (VInt x) = show x
    show (VLong x) = show x
    show (VFloat x) = show x
    show (VDouble x) = show x
    show (VReference x) = printf "0x%X" x

  instance Bits Value where
    shift (VInt x) = VInt . shift x
    shift _ = error "Bad Op: shift"

    (.|.) (VInt x) (VInt y) = VInt (x .|. y)
    (.|.) _ _ = error "Bad Op: (.|.)"

    (.&.) (VInt x) (VInt y) = VInt (x .&. y)
    (.&.) _ _ = error "Bad Op: (.&.)"

    xor (VInt x) (VInt y) = VInt (x `xor` y)
    xor _ _ = error "Bad Op: xor"

  type Local_Variable = IORef Value

  type Operand = Value

  data Method = Method {
    method_code :: Instructions,
    method_locals :: Word16
  }

  data Field = Field {
    field_value :: Value
  }

  data Class = Class {
    constant_pool :: [CP_Info],
    method_map :: IORef (Map String Method),
    field_map :: IORef (Map String Field)
  }

  type Bitmap = [Word8]

  data Runtime_Environment = Environment {
    current_class :: IORef Class,
    class_map :: IORef (Map String Class),
    stack :: Stack
  }

  -- newtype Runtime = Runtime { runRT :: Runtime_Environment -> IO () }
  --
  -- execRT :: Runtime -> Runtime_Environment -> IO ()
  -- execRT = undefined
  --
  -- instance Functor Runtime where
  --   fmap f rt = Runtime $ execRT rt >=> pure . f
  --
  -- instance Monad Runtime where
  --   rt >>= f = Runtime $ \env -> runRT rt env >> f



  data Stack_Frame = Frame {
    local_variables :: [Local_Variable],
    operand_stack :: IORef [Operand],
    code_segment :: Code_Segment
  }
