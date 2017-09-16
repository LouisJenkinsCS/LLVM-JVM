module Runtime.Runtime where
  import Control.Monad
  import Control.Monad.Reader
  import Control.Monad.Trans.Class
  import Control.Monad.IO.Class

  import Data.Vector.Unboxed (Vector)
  import Data.Maybe
  import Data.Either
  import Data.IORef
  import Data.Array.IO
  import Data.Map.Strict (Map)
  import Data.Word

  import qualified Data.Vector.Unboxed as UnboxedVector
  import qualified Data.Vector as BoxedVector
  import qualified Data.Map.Strict as Map
  import qualified Data.ByteString.Lazy as LazyByteString
  import qualified Runtime.Parser as Parser

  import Unsafe.Coerce

  import GHC.Float

  import Runtime.Parser (parseClassFile, CPConstant, ClassFile)
  import qualified Runtime.Parser as Parser


{-------------------------------------------------------------------------------
- TODO: Document for Runtime...
-------------------------------------------------------------------------------}

  -- The runtime state.
  type Runtime = ReaderT RuntimeState IO

  debugRuntime :: Bool
  debugRuntime = True

  getRuntimeState :: Runtime RuntimeState
  getRuntimeState = ask

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

  -- A bytecode instruction is a simple 8-bit word; multiple bytecode can be combined
  -- to things such as an offset or an index
  type ByteCode = Int

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
    code :: Vector ByteCode
  }

  -- Fetch and Modify the program counter
  fetchAndModifyPC :: Frame -> (Int -> (Int, Int)) -> Runtime Int
  fetchAndModifyPC frame f = liftIO $ atomicModifyIORef' (pc frame) f

  -- Convenience method to increment
  incrementPC :: Frame -> Runtime Int
  incrementPC = flip fetchAndModifyPC (\pc' -> (pc' + 1, pc'))

  -- Obtain the next byte code instruction.
  getInstruction :: Runtime ByteCode
  getInstruction = do
    frame <- currentFrame
    idx <- incrementPC frame

    -- We use bounds checking only when we are debugging
    if debugRuntime then
      code frame `UnboxedVector.indexM` fromIntegral idx
    else
      code frame `UnboxedVector.unsafeIndexM` fromIntegral idx

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
    classLoader :: ClassLoader
  }

{-------------------------------------------------------------------------------
- TODO: Document for Runtime ClassLoader...
-------------------------------------------------------------------------------}

  type Type = Int

  -- Types
  referenceType :: Type
  referenceType = 1

  intType :: Type
  intType = 2

  longType :: Type
  longType = 3

  floatType :: Type
  floatType = 4

  doubleType :: Type
  doubleType = 5

  stringType :: Type
  stringType = 6

  data ClassLoader = ClassLoader {
    -- We manage all class file instances here...
    classMapping :: IORef (Map String ClassDescriptor)
  }

  getClassLoader :: Runtime ClassLoader
  getClassLoader = classLoader <$> getRuntimeState

  loadClass :: String -> Runtime ClassDescriptor
  loadClass className = do
    loader <- getClassLoader
    mappings <- liftIO . readIORef $ classMapping loader

    case Map.lookup className mappings of
        -- Already parsed and cached
        Just classDesc -> return classDesc

        -- If the class is not present, then we must load it
        Nothing -> do
          classFile <- getClassFile
          classDesc <- parseClass classFile
          liftIO . writeIORef (classMapping loader) $ Map.insert className classDesc mappings
          return classDesc
          where
            -- Loads classfile and parses it
            getClassFile :: Runtime ClassFile
            getClassFile = do
              -- Attempt to locate the class file first...
              classContents <- liftIO $ LazyByteString.readFile className
              let maybeClassFile = parseClassFile className classContents
              let classFile = either
                              -- ParseError: Print error message (and quit)
                              (error . show)
                              -- ClassFile: Return it
                              id
                              -- Result from parse
                              maybeClassFile
              return classFile

            -- Converts parsed class file to runtime descriptor
            parseClass :: ClassFile -> Runtime ClassDescriptor
            parseClass classFile = do
              -- The class name must be obtained directly from the constant pool
              let cpool = BoxedVector.fromList $ Parser.constantPool classFile
              let
                  -- Obtain `nameIndex` field from CPClass
                  cpclass = cpool `BoxedVector.unsafeIndex` Parser.thisClassIdx classFile
                  -- Obtain 'utf8Bytes' field from CPUtf8
                  cputf8 = cpool `BoxedVector.unsafeIndex` (fromIntegral . Parser.nameIndex) cpclass
                  -- Convert raw UTF8 Bytes into a String
                  className' = map (toEnum . fromEnum) (Parser.utf8Bytes cputf8)

              return ClassDescriptor {className = className'}

  -- Obtains a constant from the constant pool
  getConstant :: (Integral a) => a -> Runtime Variable
  getConstant = undefined

  -- Runtime version of parsed Class
  data ClassDescriptor = ClassDescriptor {
    className :: String,
    methodMapping :: Map String MethodDescriptor,
    fieldMapping :: Map String FieldDescriptor,
    runtimeConstants :: BoxedVector.Vector CPConstant
  }

  -- Runtime version of parsed Method
  data MethodDescriptor = MethodDescriptor {
    -- Name of method.
    methodName :: String,
    -- Return type
    returnType :: Type,
    -- Argument types
    argsType :: [Type],
    -- Executable code
    methodCode :: [ByteCode],
    -- Local variable array size
    maxLocals :: Int,
    -- Operand stack size
    maxOperands :: Int
  }

  -- Runtime version of parsed Field
  data FieldDescriptor = FieldDescriptor {
    -- Name of field.
    fieldName :: String,
    -- Field type
    fieldType :: Type
  }
