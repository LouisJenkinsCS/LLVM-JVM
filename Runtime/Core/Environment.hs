module Runtime.Core.Environment where
  import Runtime.Core.Frame
  import Runtime.Core.Stack

  import Control.Monad.Trans.Class
  import Control.Monad
  import Control.Monad.Reader

  {-
    The Runtime Environment for the JVM, which encompasses all state.
  -}

  {-
    Types
  -}

  data Environment = Environment {
    -- TODO
  }

  -- The runtime state.
  type Runtime = ReaderT Environment IO

  -- Convenience typeclass to allow easy access...
  class (Monad m) => RuntimeEnvironment m where
    currentFrame :: m Frame
    stackFrame :: m Stack
