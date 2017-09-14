module Runtime.Runtime where
  import Control.Monad
  import Control.Monad.Reader
  import Control.Monad.Trans.Class
  import Control.Monad.IO.Class

  import Data.Vector.Unboxed
  import Data.Maybe
  import Data.IORef
  import Data.Array.IO
  import Data.Map.Strict
  import Data.Word

  import Unsafe.Coerce

  import GHC.Float

  import Runtime.Parser (parseClassFile)

{-------------------------------------------------------------------------------
- TODO: Document for Runtime...
-------------------------------------------------------------------------------}

  -- The runtime state.
  type Runtime = ReaderT RuntimeState IO

  debugRuntime :: Bool
  debugRuntime = True


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

{-------------------------------------------------------------------------------
- TODO: Document for Runtime Stack...
-------------------------------------------------------------------------------}

  type Stack = IORef [Frame]

  currentFrame :: Runtime Frame
  currentFrame = undefined

  -- Creates a new method based on the passed descriptor
  pushFrame :: MethodDescriptor -> Runtime ()
  pushFrame = undefined

  -- Pops the current frame off the stack.
  popFrame :: Runtime ()
  popFrame = undefined

{-------------------------------------------------------------------------------
- TODO: Document for Runtime Stack Frame...
-------------------------------------------------------------------------------}

  -- A variable is a generic 64-bit word that can be coerced by Haskel's compiler
  -- to the requested type.
  type Variable = Int

  {- Variable Type Conversions -}

  asFloat :: Variable -> Float
  asFloat = unsafeCoerce

  asLong :: Variable -> Integer
  asLong = unsafeCoerce

  asInt :: Variable -> Int
  asInt = unsafeCoerce

  asDouble :: Variable -> Double
  asDouble = unsafeCoerce

  {- Double Type Conversions -}

  doubleToInt :: Variable -> Int
  doubleToInt = double2Int . asDouble

  doubleToFloat :: Variable -> Float
  doubleToFloat = double2Float . asDouble

  doubleToLong :: Variable -> Integer
  doubleToLong = toInteger . double2Int . asDouble

  {- Float Type Conversions -}

  floatToInt :: Variable -> Int
  floatToInt = float2Int . asFloat

  floatToDouble :: Variable -> Double
  floatToDouble = float2Double . asFloat

  floatToLong :: Variable -> Integer
  floatToLong = toInteger . float2Int . asFloat

  {- Int Type Conversions -}

  intToFloat :: Variable -> Float
  intToFloat = unsafeCoerce . asInt

  intToDouble :: Variable -> Double
  intToDouble = unsafeCoerce . asInt

  intToLong :: Variable -> Integer
  intToLong = toInteger . asInt


  data Frame = Frame {
    variables :: IOUArray Int Variable,
    operands :: IOUArray Int Variable,
    operandTop :: IORef Int,
    pc :: IORef Int,
    code :: Vector Word8
  }

  -- Fetch and Modify the program counter
  fetchAndModifyPC :: Frame -> (Int -> (Int, Int)) -> Runtime Int
  fetchAndModifyPC frame f = liftIO $ atomicModifyIORef' (pc frame) f

  -- Convenience method to increment
  incrementPC :: Frame -> Runtime Int
  incrementPC = flip fetchAndModifyPC (\pc' -> (pc' + 1, pc'))

  -- Obtain the next byte code instruction.
  getInstruction :: Runtime Word8
  getInstruction = do
    frame <- currentFrame
    idx <- incrementPC frame

    -- We use bounds checking only when we are debugging
    if debugRuntime then
      code frame `indexM` fromIntegral idx
    else
      code frame `unsafeIndexM` fromIntegral idx

  load :: Integral a => a -> Runtime Variable
  load idx = do
    frame <- currentFrame
    liftIO $ readArray (variables frame) (fromIntegral idx)

  store :: Integral a => a -> Variable -> Runtime ()
  store idx var = do
    frame <- currentFrame
    liftIO $ writeArray (variables frame) (fromIntegral idx) var

  modify :: Integral a => a -> (Variable -> Variable) -> Runtime ()
  modify idx f = do
    var <- load idx
    store idx (f var)

  push :: Variable -> Runtime ()
  push var = do
    frame <- currentFrame
    liftIO $ do
      idx <- atomicModifyIORef' (operandTop frame) (\x -> (x + 1, x))
      writeArray (operands frame) idx var

  pop :: Runtime Variable
  pop = do
    frame <- currentFrame
    liftIO $ do
      idx <- atomicModifyIORef' (operandTop frame) (\x -> (x - 1, x - 1))
      readArray (operands frame) idx

{-------------------------------------------------------------------------------
- TODO: Document for Runtime Environment State...
-------------------------------------------------------------------------------}

  data RuntimeState = RuntimeState {
    -- TODO
  }

{-------------------------------------------------------------------------------
- TODO: Document for Runtime ClassLoader...
-------------------------------------------------------------------------------}

  data ClassLoader = ClassLoader {
    -- We manage all class file instances here...
  }

  -- Runtime version of parsed Class
  data ClassDescriptor = ClassDescriptor {
    className :: String,
    methodMapping :: Map String MethodDescriptor,
    fieldMapping :: Map String FieldDescriptor
  }

  -- Obtains a constant from the constant pool
  getConstant :: (Integral a) => a -> m ConstantDescriptor
  getConstant = undefined

  -- Runtime version of parsed Method
  data MethodDescriptor = MethodDescriptor {

  }

  -- Runtime version of parsed Field
  data FieldDescriptor = FieldDescriptor {

  }

  -- Runtime version of parsed Constant
  data ConstantDescriptor = ConstantDescriptor {

  }
