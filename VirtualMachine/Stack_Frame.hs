module VirtualMachine.Stack_Frame where
  data Stack_Frame = Frame {
    locals :: [Local_Variables],
    opStack :: [Operands]
  }

  {-
    Pushes a value as a local variable at the given index.
  -}
  putLocal :: Word16 -> Value -> Stack_Frame -> Stack_Frame

  {-
    Returns the value associated the given index.
  -}
  getLocal :: Word16 -> Stack_Frame -> Value

  {-
    Pushes a value on the operand stack
  -}
  pushOp :: Value -> Stack_Frame -> Stack_Frame

  {-
    Pops the operand off of the stack
  -}
  popOp :: Stack_Frame -> (Value, Stack_Frame)

  {-
    Pops off N operands off of the stack
  -}
  popOpN :: Word8 -> Stack_Frame -> ([Value], Stack_Frame)
