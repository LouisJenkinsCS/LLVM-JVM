module VirtualMachine.Stack where
  import Data.IORef
  import Control.Monad
  import Data.Word
  import VirtualMachine.Types
  import VirtualMachine.Stack_Frame
  import VirtualMachine.ByteCode

  getCurrentFrame :: Runtime_Environment -> IO StackFrame
  getCurrentFrame env = head <$> readIORef (stack env)

  {-
    Pops the current frame off of the stack. No epilogues, such as garbage collection,
    have been implemented yet.
  -}
  popFrame :: Runtime_Environment -> IO ()
  popFrame env = modifyIORef' (stack env) tail

  {-
    Creates a stack frame with the request number of Local Variables and the
    executable bytecode instructions. The number of Local Variables are static and must
    support random-access, but the Operand Stack is best implemented as a mutable
    reference.
  -}
  pushFrame :: Runtime_Environment -> Method -> IO ()
  pushFrame env meth = createFrame meth >>= \f -> modifyIORef (stack env) ((:) f)
