module VirtualMachine.Stack where
  import Data.IORef
  import Control.Monad
  import Data.Word
  import VirtualMachine.Types
  import VirtualMachine.Stack_Frame
  import VirtualMachine.ByteCode

  getCurrentFrame :: Stack -> IO StackFrame
  getCurrentFrame stack = head <$> readIORef stack

  {-
    Pops the current frame off of the stack. No epilogues, such as garbage collection,
    have been implemented yet.
  -}
  popFrame :: Stack -> IO ()
  popFrame stack = modifyIORef' stack tail

  pushFrame :: Stack -> Word16 -> [ByteCode] -> IO ()
  pushFrame stack maxLocals instr = readIORef stack
    >>= \s -> createFrame instr maxLocals >>= newIORef
    >>= \n -> writeIORef stack (n : s)

  debugStack :: Stack -> IO String
  debugStack stack = readIORef stack >>= \s -> debugFrame' (length s) s
    where
      debugFrame' :: Int -> [StackFrame] -> IO String
      debugFrame' idx frames
        | idx == 0 = return ""
        | otherwise = do
            str  <- debugFrame $ frames !! (idx - 1)
            next <- debugFrame' (idx-1) frames
            return $ str ++ "," ++ next

  debugExec :: Stack -> IO ()
  debugExec stack = getCurrentFrame stack >>= execute
