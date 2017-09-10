{-# LANGUAGE MultiParamTypeClasses, DuplicateRecordFields, FlexibleInstances #-}
module Parser.Class.Types where
  import Text.Parsec (Parsec, getState, putState)
  import Data.ByteString.Lazy
  import Data.Word (Word8, Word16, Word32)

  {-
    ClassFile Types
  -}

  -- Parsed representation of a '.class' file's constant-pool constants.
  data CPConstant = CPClass { nameIndex :: Word16 }
    | CPFieldref { classIndex :: Word16, nameAndTypeIndex :: Word16 }
    | CPMethodref { classIndex :: Word16, nameAndTypeIndex :: Word16 }
    | CPInterfaceMethodref { classIndex :: Word16, nameAndTypeIndex :: Word16 }
    | CPString { stringIndex :: Word16 }
    | CPInteger { bytes :: Word32 }
    | CPFloat { bytes :: Word32 }
    | CPLong { highBytes :: Word32, lowBytes :: Word32 }
    | CPDouble { highBytes :: Word32, lowBytes :: Word32 }
    | CPNameAndType { nameIndex :: Word16, descriptorIndex :: Word16 }
    | CPUtf8 { utf8Bytes :: [Word8] }
    | CPMethodHandle { referenceKind :: Word8, referenceIndex :: Word16 }
    | CPMethodType { descriptorIndex :: Word16 }
    | CPInvokeDynamic { bootstrapMethodAttrIndex :: Word16, nameAndTypeIndex :: Word16 }
    | CPDummy
      deriving Show

  -- Parsed representation of a '.class' file's fields.
  data Field = Field {
    accessFlags :: Word16,
    nameIndex :: Word16,
    descriptorIndex :: Word16,
    attributes :: [Attribute]
  } deriving Show

  -- Parsed representation of a '.class' file's methods.
  data Method = Method {
    accessFlag :: Word16,
    nameIndex :: Word16,
    descriptorIndex :: Word16,
    attributes :: [Attribute]
  } deriving Show

  -- Parsed representation of a '.class' file.
  data ClassFile = ClassFile {
    constantPool :: [CPConstant],
    interfaces :: [Word16],
    fields :: [Field],
    methods :: [Method],
    attributes :: [Attribute]
  } deriving Show

  data ExceptionEntry = ExceptionEntry {
    startPc :: Word16,
    endPc :: Word16,
    handlerPc :: Word16,
    catchType :: Word16
  } deriving (Show)

  -- Attribute Types
  data Attribute = UnknownAttribute { attributeName :: String, attributeInfo :: [Word8] }
    | ConstantValue { constantValueIndex :: Word16 }
    | CodeAttribute {
      maxStack :: Word16,
      maxLocals :: Word16,
      code :: [Word8],
      exceptionTable :: [ExceptionEntry],
      attributes :: [Attribute]
    } deriving (Show)

  {-
    Parsec Types
  -}

  -- Our custom parser type. We operate on lazy ByteStrings (which allows us to
  -- process it it in chunks, as well as only load parts of the '.class' file on
  -- demand). We also maintain a 'State' object.
  type Parser = Parsec ByteString ParserState

  class (Monad m) => ParserState_ m where
    getConstantPool :: m [CPConstant]
    setConstantPool :: [CPConstant] -> m ()
    getStatus :: m Int
    setStatus :: Int -> m ()

  -- The state carried forward through parsing.
  data ParserState = ParserState {
    -- The Constant Pool.
    constantPool :: [CPConstant],
    -- Current state encoded as a number.
    status :: Int
  }

  -- Constant statuses...
  skip :: Int
  skip = 1

  normal :: Int
  normal = 0

  instance ParserState_ Parser where
    getConstantPool = do
      state <- getState
      return $ constantPool (state :: ParserState)

    -- Update constant pool after it gets parsed, so it becomes accessible
    setConstantPool cpool = do
      state <- getState
      putState $ ParserState cpool (status (state :: ParserState))

    getStatus = do
      state <- getState
      return $ status (state :: ParserState)

    -- Update status, but carry the constant pool forward.
    setStatus newStatus = do
      state <- getState
      putState $ ParserState (constantPool (state :: ParserState)) newStatus
