module VirtualMachine.Stack_Frame where
  import Data.Word
  import Control.Lens
  import Data.IORef
  import Control.Monad
  import VirtualMachine.Types

  debugFrame :: StackFrame -> IO String
  debugFrame frameRef = do
    frame <- readIORef frameRef
    locals' <- readIORef $ locals frame
    opStack' <- readIORef $ opStack frame
    instr <- readIORef $ instructions frame
    return $ "Stack_Frame{locals:" ++ show locals' ++ ",opStack:" ++ show opStack' ++ ",instr:" ++ show instr ++ "}";

  createFrame :: [ByteCode] -> IO Stack_Frame
  createFrame instr = do
    emptyLocals <- newIORef []
    emptyOpStack <- newIORef []
    instr' <- newIORef instr
    return $ Frame emptyLocals emptyOpStack instr'

  {-
    Pushes a value as a local variable at the given index.
  -}
  putLocal :: Word16 -> Value -> StackFrame -> IO ()
  putLocal offset val frameRef = do
    frame <- readIORef frameRef
    let l = locals frame
    old <- readIORef l
    writeIORef l ((element (fromEnum offset) .~ val) old)

  {-
    Returns the value associated the given index.
  -}
  getLocal :: Word16 -> StackFrame -> IO Local_Variable
  getLocal offset frameRef = do
    frame <- readIORef frameRef
    let l = locals frame
    arr <- readIORef l
    return $ arr !! fromEnum offset

  {-
    Pushes a value on the operand stack
  -}
  pushOp :: Value -> StackFrame -> IO ()
  pushOp val frameRef = do
    frame <- readIORef frameRef
    let o = opStack frame
    modifyIORef' o (\old -> val : old)

  {-
    Pops the operand off of the stack
  -}
  popOp :: StackFrame -> IO Value
  popOp frameRef = do
    frame <- readIORef frameRef
    let o = opStack frame
    arr <- readIORef o
    let val = head arr
    modifyIORef' o tail
    return val

  {-
    Pops off N operands off of the stack
  -}
  popOpN :: Word8 -> StackFrame -> IO [Value]
  popOpN n frameRef = replicateM (fromEnum n) (popOp frameRef)
