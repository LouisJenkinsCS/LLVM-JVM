{-# LANGUAGE FlexibleInstances #-}
module Runtime.Core.Types where
  import Control.Monad
  import Control.Monad.Reader
  import Control.Monad.Trans.Class
  import Control.Monad.IO.Class

  import Data.IORef
  import Data.Array.IO

  import Parser.Class.ClassFile (ClassFile, parseClassFile)

  type Variable = Int
  type Stack = IOUArray Int Frame

  -- Encapsulates a stack frame.
  data Frame = Frame {
    variables :: IOUArray Int Variable,
    operands :: IOUArray Int Variable,
    operandTop :: IORef Int,
    pc :: IORef Int
  }

  data Environment = Environment {
    -- TODO
  }

  data ClassLoader = ClassLoader {
    -- We manage all class file instances here...

  }

  -- The runtime state.
  type Runtime = ReaderT Environment IO

  -- Convenience typeclass to allow easy access...
  class (Monad m) => RuntimeEnvironment m where
    currentFrame :: m Frame
    stackFrame :: m Stack

  instance RuntimeEnvironment Runtime where
    currentFrame = undefined
    stackFrame = undefined
