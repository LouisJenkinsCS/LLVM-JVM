module VirtualMachine.Stack_Frame where
  import Data.Word
  import Data.Bits
  import Data.IORef
  import Control.Monad
  import VirtualMachine.Types
  import Data.Array.IO

  {-
    Constructs a stack frame from the passed method. Each stack frame keeps track
    of it's local variables, operand stack, and code segment, which is composed
    of the instructions and the current program counter.
  -}
  createFrame :: Method -> IO StackFrame
  createFrame meth = createFrame' >>= newIORef
    where
      createFrame' :: IO Stack_Frame
      createFrame' = newIORef ([] :: [Operand]) >>= \opstack -> newIORef 0 >>=
        \pc -> (createLocals . method_locals) meth >>= \locals ->
        return Frame {
          local_variables = locals,
          operand_stack = opstack,
          code_segment = Code {
            byte_code = method_code meth,
            program_counter = pc
          }
        }
          where
            createLocals :: Word16 -> IO [Local_Variable]
            createLocals n
              | n == 0 = return []
              | n > 0 = (:) <$> newIORef (VReference 0) <*> createLocals (n - 1)
              | otherwise = error $ "Error while attempting to create locals: n=" ++ show n

  getPC :: StackFrame -> IO (IORef Word32)
  getPC frame = program_counter . code_segment <$> readIORef frame

  getPC' :: Integral a => StackFrame -> IO a
  getPC' frame = getPC frame >>= \f -> fromIntegral <$> readIORef f

  setPC :: Integral a => StackFrame -> a -> IO ()
  setPC frame pc = getPC frame >>= flip writeIORef (fromIntegral pc)

  modifyPC :: Integral a => StackFrame -> (a -> a) -> IO ()
  modifyPC frame f = (f <$> getPC' frame) >>= setPC frame >> getPC' frame >>= print

  maxPC :: Integral a => StackFrame -> IO a
  maxPC frame = fromIntegral . length <$> getInstructions frame

  getInstructions :: StackFrame -> IO Instructions
  getInstructions frame = byte_code . code_segment <$> readIORef frame

  {-
    Pushes a value on the operand stack
  -}
  pushOp :: StackFrame -> Value -> IO ()
  pushOp frame val = (operand_stack <$> readIORef frame) >>= flip modifyIORef (val :)

  {-
    Pops the operand off of the stack
  -}
  popOp :: StackFrame -> IO Value
  popOp frame = readIORef frame >>= \f -> readIORef (operand_stack f)
    >>= \ops -> writeIORef (operand_stack f) (tail ops) >> return (head ops)

  {-
    Pushes a value as a local variable at the given index.
  -}
  putLocal :: (Integral a) => StackFrame -> a -> Value -> IO ()
  putLocal frame idx val = readIORef frame >>= \f -> writeIORef (local_variables f !! fromIntegral idx) val

  {-
    Returns the value associated the given index.
  -}
  getLocal :: (Integral a) => StackFrame -> a -> IO Value
  getLocal frame idx = readIORef frame >>= \f -> readIORef (local_variables f !! fromIntegral idx)

  {-
    Pops off N operands off of the stack
  -}
  popOpN :: Word8 -> StackFrame -> IO [Operand]
  popOpN n frameRef = replicateM (fromEnum n) (popOp frameRef)
