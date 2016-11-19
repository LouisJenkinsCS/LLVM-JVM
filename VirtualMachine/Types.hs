module VirtualMachine.Types where
  import Data.Word
  import Data.IORef
  import Data.Bits
  import Data.Map
  import Data.Array.IO
  import ClassFile.Types
  import Control.Monad
  import Data.STRef

  type Stack = IORef [StackFrame]

  type StackFrame = IORef (Stack_Frame Int)

  type ByteCode = Word8
  type Instructions = [ByteCode]

  data Code_Segment = Code {
    byte_code :: Instructions,
    program_counter :: IORef Word32
  }

  -- Temporary type until a proper heap is setup
  type Object = Int

  -- LV_Reference gets garbage collected by Haskell's GC.
  data Value = VInt Int | VLong Integer| VFloat Float | VDouble Double | VReference Object deriving (Eq, Ord, Show)


  type Local_Variable = Value

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
    class_map :: IORef (Map String Class),
    stack :: Stack,
    debugMsgs :: [String]
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



  data Stack_Frame a = Frame {
    local_variables :: IOArray a Local_Variable,
    operand_stack :: IORef [Operand],
    code_segment :: Code_Segment
  }
