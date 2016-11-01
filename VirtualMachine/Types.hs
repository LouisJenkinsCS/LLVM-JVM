module VirtualMachine.Types where
  import Data.Word
  import Data.IORef

  type Stack = IORef [StackFrame]

  type StackFrame = IORef Stack_Frame

  type ByteCode = Word8
  type Instructions = IORef [ByteCode]

  type Local_Variable = Word64
  type Operand = Word32
  type Value = Word64

  data Stack_Frame = Frame {
    locals :: IORef [Local_Variable],
    opStack :: IORef [Operand],
    instructions :: Instructions
  }
