module VirtualMachine.Environment where
  import Data.IORef
  import GHC.IOArray
  import Data.Word
  import Data.Map
  import Control.Monad.State
  import VirtualMachine.Types

  type Runtime_Environment = State Environment

  data Class = Class

  data Environment = Env {
    class_map :: IORef (Map String Class),
    stack_frame :: StackFrame,
    heap :: IOArray Int Word8
  }

 -- {-
 --  Executes next calling instruction
 -- -}
 -- nextInstruction :: STRef Environment -> ST Environment
 -- nextInstruction env = runST $ do
 --
 --
 -- {-
 --  Pops off the current frame
 -- -}
 -- popFrame :: Environment -> Environment
 --
 -- {-
 --  Pushes the new frame
 -- -}
 -- pushFrame :: Stack_Frame -> Environment -> Environment
 --
 -- {-
 --  Updates the current frame
 -- -}
 -- updateFrame :: Stack_Frame -> Environment -> Environment
 --
 -- {-
 --  Obtains the current frame
 -- -}
 -- currentFrame :: Environment -> Stack_Frame
 --
 -- {-
 --  Advance program counter
 -- -}
 -- advancePC :: Environment -> Environment

 {-
  Set the current program counter
 -}
 -- setPC :: STRef s Environment -> PC -> ()
 -- setPC env pc' = runST $ do
 --   modifySTRef (pc env) (\_ -> pc')
