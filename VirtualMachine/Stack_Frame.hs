module VirtualMachine.Stack_Frame where
  import Data.Word
  import Data.Bits
  import Data.IORef
  import Control.Monad
  import VirtualMachine.Types
  import Data.Array.IO

  {- |
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

  {- | Obtain the reference to the PC of this stack frame -}
  getPC :: StackFrame -> IO (IORef Word32)
  getPC frame = program_counter . code_segment <$> readIORef frame

  {- | Obtain the PC of this stack frame -}
  getPC' :: Integral a => StackFrame -> IO a
  getPC' frame = getPC frame >>= \f -> fromIntegral <$> readIORef f

  {- | Set the PC of this stack frame -}
  setPC :: Integral a => StackFrame -> a -> IO ()
  setPC frame pc = getPC frame >>= flip writeIORef (fromIntegral pc)

  {- | Mutate the PC of this stack frame -}
  modifyPC :: Integral a => StackFrame -> (a -> a) -> IO ()
  modifyPC frame f = (f <$> getPC' frame) >>= setPC frame

  {- | Maximum PC for this stack frame (without going out of bounds) -}
  maxPC :: Integral a => StackFrame -> IO a
  maxPC frame = fromIntegral . length <$> getInstructions frame

  getInstructions :: StackFrame -> IO Instructions
  getInstructions frame = byte_code . code_segment <$> readIORef frame

  {- | Pushes a value on the operand stack -}
  pushOp :: StackFrame -> Value -> IO ()
  pushOp frame val = (operand_stack <$> readIORef frame) >>= flip modifyIORef (val :)

  {- | Pops the operand off of the stack -}
  popOp :: StackFrame -> IO Value
  popOp frame = readIORef frame >>= \f -> readIORef (operand_stack f)
    >>= \ops -> writeIORef (operand_stack f) (tail ops) >> return (head ops)

  {- |
    Pushes a value as a local variable at the given index.
  -}
  putLocal :: (Integral a) => StackFrame -> a -> Value -> IO ()
  putLocal frame idx val = getLocal frame idx >>= flip writeIORef val

  modifyLocal :: (Integral a) => StackFrame -> a -> (Value -> Value) -> IO ()
  modifyLocal frame idx f = getLocal frame idx >>= flip modifyIORef f

  {- | Returns the value of the local variable. -}
  getLocal' :: (Integral a) => StackFrame -> a -> IO Value
  getLocal' frame idx = readIORef frame >>= \f -> readIORef (local_variables f !! fromIntegral idx)

  {- | Returns a reference to the local variable -}
  getLocal :: Integral a => StackFrame -> a -> IO (IORef Value)
  getLocal frame idx = (!! fromIntegral idx) . local_variables <$> readIORef frame
