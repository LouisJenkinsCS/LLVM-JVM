module VirtualMachine.Environment where
  import Data.STRef
  import Control.Monad.ST
  import VirtualMachine.Method_Area
  import VirtualMachine.Stack_Frame

  data Environment = Env {
    method_area :: STRef Method_Area,
    stack_frame :: STRef [Stack_Frame],
    heap :: STRef Heap,
    pc :: STRef PC
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
