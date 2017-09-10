{-# LANGUAGE FlexibleInstances #-}
module Runtime.Core.Types where
  import Control.Monad
  import Control.Monad.Reader
  import Control.Monad.Trans.Class
  import Control.Monad.IO.Class

  import Data.IORef
  import Data.Array.IO
  import Data.Map.Strict
  import Data.Word

  import Parser.Class.ClassFile (parseClassFile)

  type Variable = Int
  type Stack = IORef [Frame]

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

  -- Runtime version of parsed Class
  data ClassDescriptor = ClassDescriptor {
    className :: String,
    methodMapping :: Map String MethodDescriptor,
    fieldMapping :: Map String FieldDescriptor
  }

  -- Runtime version of parsed Method
  data MethodDescriptor = MethodDescriptor {

  }

  -- Runtime version of parsed Field
  data FieldDescriptor = FieldDescriptor {

  }

  -- Runtime version of parsed Constant
  data ConstantDescriptor = ConstantDescriptor {

  }

  -- The runtime state.
  type Runtime = ReaderT Environment IO

  {-
    Ideal Abstraction:

      bc <- getInstruction
      when (bc == 0x10) do
        -- Next two byte code determine target method...
        hi <- getInstruction
        lo <- getInstruction
        targetMethod <- asMethod . getConstant $ (hi << 8) | lo
        pushFrame targetMethod
  -}

  -- Convenience typeclass to allow easy access...
  class (Monad m) => RuntimeEnvironment m where
    -- Obtain current stack frame
    currentFrame :: m (Maybe Frame)
    -- Creates a new method based on the passed descriptor
    pushFrame :: MethodDescriptor -> m ()
    -- Pops the current frame off the stack.
    popFrame :: m ()
    -- Obtains a constant from the constant pool
    getConstant :: (Integral a) => a -> m ConstantDescriptor
    -- Obtain the next byte code instruction
    getInstruction :: m Word8

  instance RuntimeEnvironment Runtime where
    currentFrame = undefined
