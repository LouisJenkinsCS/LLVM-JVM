-- 's' could be the actual state itself of the Runtime_Environment. I'm using IORef
-- almost exclusively currently, as everything needs to be mutable. In light of all
-- of this, I just might switch over to STRef for now, since I'll most likely be want
-- escaping from the IO monad. What I want to do is have the first parameter (or any order)
-- being the state, be passed implicitly when these functions are called.
class Environment s where
  -- Should take a String, and add it to a queue to be output after the current instruction is processed.
  debug :: s -> String -> ()

  -- Should delegate processing to it's components, but this data still needs to be accessible anywhere.
  -- In particular, it will get the constant pool info of the current class at the requested indice.
  getConstantPool :: s -> Integral a -> CP_Info

  -- Etc...
