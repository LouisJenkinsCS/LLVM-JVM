module Runtime.Stack where
  import Data.IORef
  import Control.Monad
  import Data.Word
  import Runtime.Types
  import Runtime.Stack_Frame
  import Runtime.ByteCode

  {- | Obtains the current stack frame -}
  getCurrentFrame :: Runtime_Environment -> IO StackFrame
  getCurrentFrame env = head <$> readIORef (stack env)

  {- | Pops the current stack frame off of the stack -}
  popFrame :: Runtime_Environment -> IO ()
  popFrame env = modifyIORef' (stack env) tail

  {- | Create a frame for the passed method and pushes on stack -}
  pushFrame :: Runtime_Environment -> Method -> IO ()
  pushFrame env meth = createFrame meth >>= \f -> modifyIORef (stack env) ((:) f)
