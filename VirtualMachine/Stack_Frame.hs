module VirtualMachine.Stack_Frame where
  import Data.Word
  import Data.Bits
  import Data.IORef
  import Control.Monad
  import VirtualMachine.Types
  import Data.Array.MArray

  debugFrame :: StackFrame -> IO String
  debugFrame frame = readIORef frame >>= \f -> getElems (local_variables f)
    >>= \l -> readIORef (operand_stack f) >>= \o ->  let i = byte_code . code_segment $ f in
    return $ "Stack_Frame{locals:" ++ show l ++ ",opStack:" ++ show o ++ ",instr:" ++ show i ++ "}"

  {-
    Creates a stack frame with the request number of Local Variables and the
    executable bytecode instructions. The number of Local Variables are static and must
    support random-access, but the Operand Stack is best implemented as a mutable
    reference.
  -}
  pushFrame :: Runtime_Environment -> Method -> IO ()
  pushFrame env meth = modifyIORef (stack env) ((:) createFrame)
    where
      createFrame :: StackFrame
      createFrame = undefined


  {-
    Pushes a value on the operand stack
  -}
  pushOp :: StackFrame -> Operand -> IO ()
  pushOp frame val = readIORef frame >>= \f -> modifyIORef (operand_stack f) (\old -> val : old)

  {-
    Pops the operand off of the stack
  -}
  popOp :: StackFrame -> IO Operand
  -- IORef can also be modified via 'writeIORef'
  popOp frame = readIORef frame >>= \f -> readIORef (operand_stack f)
    >>= \ops -> writeIORef (operand_stack f) (tail ops) >> return (head ops)

  {-
    Pushes a value as a local variable at the given index.
  -}
  putLocal :: (Integral a) => StackFrame -> a -> Value -> IO ()
  putLocal frame idx val = readIORef frame >>= \f -> writeArray (local_variables f) (fromIntegral idx) val

  {-
    Returns the value associated the given index.
  -}
  getLocal :: (Integral a) => StackFrame -> a -> IO Local_Variable
  getLocal frame idx = readIORef frame >>= \f -> readArray (local_variables f) (fromIntegral idx)

  {-
    Pops off N operands off of the stack
  -}
  popOpN :: Word8 -> StackFrame -> IO [Operand]
  popOpN n frameRef = replicateM (fromEnum n) (popOp frameRef)
