module VirtualMachine.Types where
  import Data.Word
  import Data.IORef
  import Data.Bits
  import Data.Map
  import Data.Array.IO
  import ClassFile.Types

  type Stack = IORef [StackFrame]

  type StackFrame = IORef (Stack_Frame Word16)

  type ByteCode = Word8
  type Instructions = [ByteCode]

  data Code_Segment = Code {
    byte_code :: Instructions,
    program_counter :: IORef Word32
  }

  -- LV_Reference gets garbage collected by Haskell's GC.
  data Value = LV_Null | LV_Integer Int | LV_Long Integer | LV_Float Float | LV_Double Double | LV_Reference (IORef [Word8])

  type Local_Variable = Word32

  wordToInt :: Word32 -> Value
  wordToInt = LV_Integer . fromIntegral

  wordToLong :: Word32 -> Word32 -> Value
  wordToLong x y = LV_Long $ fromIntegral (x `shiftL` 32 .|. y)

  type Operand = Value

  data Method = Method {
    method_code :: Instructions
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
    stack :: Stack
  }

  data Stack_Frame a = Frame {
    local_variables :: IOArray a Local_Variable,
    operand_stack :: IORef [Operand],
    code_segment :: Code_Segment
  }
