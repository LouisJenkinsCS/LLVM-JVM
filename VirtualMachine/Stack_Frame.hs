module VirtualMachine.Stack_Frame where
  import Data.Word
  import Data.Bits
  import Data.IORef
  import Control.Monad
  import VirtualMachine.Types
  import Data.Array.MArray

  debugFrame :: StackFrame -> IO String
  debugFrame frame = readIORef frame >>= \f -> getElems (locals f)
    >>= \l -> readIORef (opStack f) >>= \o -> readIORef (instructions f) >>= \i ->
    return $ "Stack_Frame{locals:" ++ show l ++ ",opStack:" ++ show o ++ ",instr:" ++ show i ++ "}"

  {-
    Creates a stack frame with the request number of Local Variables and the
    executable bytecode instructions. The number of Local Variables are static and must
    support random-access, but the Operand Stack is best implemented as a mutable
    reference.
  -}
  createFrame :: [ByteCode] -> Word16 -> IO (Stack_Frame Word16)
  createFrame instr maxLocals = Frame <$> newArray (0, maxLocals -1) 0 <*> newIORef [] <*> newIORef instr

  {-
    Pushes a value on the operand stack
  -}
  pushOp :: Operand -> StackFrame -> IO ()
  pushOp val frame = readIORef frame >>= \f -> modifyIORef (opStack f) (\old -> val : old)

  {-
    Pops the operand off of the stack
  -}
  popOp :: StackFrame -> IO Operand
  -- IORef can also be modified via 'writeIORef'
  popOp frame = readIORef frame >>= \f -> readIORef (opStack f)
    >>= \ops -> writeIORef (opStack f) (tail ops) >> return (head ops)

  {-
    Pushes a value as a local variable at the given index.
  -}
  putLocal :: Word16 -> Value -> StackFrame -> IO ()
  putLocal idx val frame = readIORef frame >>= \f -> writeArray (locals f) idx val

  {-
    Returns the value associated the given index.
  -}
  getLocal :: Word16 -> StackFrame -> IO Local_Variable
  getLocal idx frame = readIORef frame >>= \f -> readArray (locals f) idx

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
    Helper function to convert to appropriate type
  -}
  popWORD :: StackFrame -> IO Word32
  popWORD frameRef = fromIntegral <$> popOp frameRef

  popDWORD :: StackFrame -> IO Word64
  popDWORD frameRef = asDWORD <$> replicateM 2 (popOp frameRef)
    where
      asDWORD (high:low:_) = fromIntegral $ high `shift` 32 .|. low
  {-
    Pops off N operands off of the stack
  -}
  popOpN :: Word8 -> StackFrame -> IO [Operand]
  popOpN n frameRef = replicateM (fromEnum n) (popOp frameRef)
