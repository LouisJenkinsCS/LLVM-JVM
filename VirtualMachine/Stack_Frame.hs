module VirtualMachine.Stack_Frame where
  import Data.Word
  import Control.Monad.ST
  import Data.STRef
  import Control.Lens

  type Local_Variables = Int
  type Operands = Int
  type Value = Int

  data Stack_Frame s = Frame {
    locals :: !(STRef s [Value]),
    opStack :: !(STRef s [Operands])
  }

  {-
    Pushes a value as a local variable at the given index.
  -}
  putLocal :: Word16 -> Value -> Stack_Frame s -> ST s ()
  putLocal offset val frame = do
    let l = locals frame
    old <- readSTRef l
    writeSTRef l ((element (fromEnum offset) .~ val) old)

  {-
    Returns the value associated the given index.
  -}
  getLocal :: Word16 -> Stack_Frame s -> ST s Value
  getLocal offset frame = do
    let l = locals frame
    arr <- readSTRef l
    return $ arr !! fromEnum offset

  -- {-
  --   Pushes a value on the operand stack
  -- -}
  -- pushOp :: Value -> Stack_Frame -> Stack_Frame
  --
  -- {-
  --   Pops the operand off of the stack
  -- -}
  -- popOp :: Stack_Frame -> (Value, Stack_Frame)
  --
  -- {-
  --   Pops off N operands off of the stack
  -- -}
  -- popOpN :: Word8 -> Stack_Frame -> ([Value], Stack_Frame)
