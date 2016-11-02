module VirtualMachine.Stack_Frame where
  import Control.Applicative
  import Data.Word
  import Data.Bits
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
    -- TODO: Change to Data.Array.MArray, so I can avoid this mess
    modifyIORef l (inject 0 offset val)
      where
        inject :: Word16 -> Word16 -> Value -> [Local_Variable] -> [Local_Variable]
        inject m n val' (x:xs)
          | n == m =  val' : x : xs
          | n > m = x : inject (m+1) n val' xs
          | otherwise = error $ "Bad range: " ++ show m ++ " of " ++ show n
        inject _ _ val' [] = [val']
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
    Obtains the next WORD-sized (4-byte, Word32) local variable from the stack.
  -}
  getLocalWORD :: Word16 -> StackFrame -> IO Word32
  getLocalWORD idx frame = fromIntegral <$> getLocal idx frame

  {-
    Obtains the next DWORD-sized (8-bytes, Word64) local variable from the stack.
    As 'locals' is segmented in WORD-sized slots (4-bytes, Word32), in big-endian
    order (higher byte first), we must obtain index 'n' and 'n+1' and combine
    them (I.E: In C -> high_word << 32 | low_word).
  -}
  getLocalDWORD :: Word16 -> StackFrame -> IO Word64
  getLocalDWORD idx frame = asDWORD <$> getLocalWORD idx frame <*> getLocalWORD (idx + 1) frame
      where
        asDWORD high low = fromIntegral $ high `shift` 32 .|. low

  {-
    Pushes a value on the operand stack
  -}
  pushOp :: Operand -> StackFrame -> IO ()
  pushOp val frameRef = do
    frame <- readIORef frameRef
    let o = opStack frame
    modifyIORef' o (\old -> fromIntegral val : old)

  {-
    Pops the operand off of the stack
  -}
  popOp :: StackFrame -> IO Operand
  popOp frameRef = do
    frame <- readIORef frameRef
    let o = opStack frame
    arr <- readIORef o
    let val = head arr
    modifyIORef' o tail
    return $ fromIntegral val

  {-
    Pops off N operands off of the stack
  -}
  popOpN :: Word8 -> StackFrame -> IO [Operand]
  popOpN n frameRef = replicateM (fromEnum n) (popOp frameRef)
