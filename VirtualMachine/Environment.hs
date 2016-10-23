module VirtualMachine.Environment where
  data Environment = Env {
    method_area :: Method_Area,
    stack_frame :: [Stack_Frame],
    heap :: Heap,
    pc :: PC
  }

 {-
  Executes next calling instruction
 -}
 nextInstruction :: Environment -> Environment

 {-
  Pops off the current frame
 -}
 popFrame :: Environment -> Environment

 {-
  Pushes the new frame
 -}
 pushFrame :: Stack_Frame -> Environment -> Environment

 {-
  Updates the current frame
 -}
 updateFrame :: Stack_Frame -> Environment -> Environment

 {-
  Obtains the current frame
 -}
 currentFrame :: Environment -> Stack_Frame

 {-
  Advance program counter
 -}
 advancePC :: Environment -> Environment

 {-
  Set the current program counter
 -}
 setPC :: Environment -> Environment
