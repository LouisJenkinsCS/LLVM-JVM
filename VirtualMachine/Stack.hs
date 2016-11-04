module VirtualMachine.Stack where
  import Data.IORef
  import Control.Monad
  import Data.Word
  import VirtualMachine.Types
  import VirtualMachine.Stack_Frame
  import VirtualMachine.ByteCode

  bootstrap :: IO Stack
  bootstrap = newIORef []

  getCurrentFrame :: Stack -> IO StackFrame
  getCurrentFrame framesRef = do
    frames <- readIORef framesRef
    return $ head frames

  popFrame :: Stack -> IO ()
  popFrame framesRef = modifyIORef' framesRef tail

  pushFrame :: Stack -> Word16 -> [ByteCode] -> IO ()
  pushFrame stack maxLocals instr = readIORef stack
    >>= \s -> createFrame instr maxLocals >>= newIORef
    >>= \n -> writeIORef stack (n : s)

  debugStack :: Stack -> IO String
  debugStack stack = do
    stack' <- readIORef stack
    let len = length stack'
    debugFrame' len stack'
      where
        debugFrame' :: Int -> [StackFrame] -> IO String
        debugFrame' idx frames
          | idx == 0 = return ""
          | otherwise = do
              str  <- debugFrame $ frames !! (idx - 1)
              next <- debugFrame' (idx-1) frames
              return $ str ++ "," ++ next

  debugExec :: Stack -> IO ()
  debugExec stack = do
    currFrame <- getCurrentFrame stack
    currFrame' <- readIORef currFrame
    execute currFrame (instructions currFrame')
