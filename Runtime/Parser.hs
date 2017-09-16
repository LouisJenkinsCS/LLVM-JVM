{-# LANGUAGE MultiParamTypeClasses, DuplicateRecordFields, FlexibleInstances #-}
module Runtime.Parser where
  import Text.Parsec (ParseError, (<?>), count, anyChar)
  import Text.Parsec.Pos
  import Text.Parsec.Prim

  import Data.Word
  import Data.Bits
  import Data.ByteString.Internal (c2w)
  import Data.ByteString.Lazy (ByteString)
  import qualified Data.Bifunctor as BiFunctor
  import qualified Data.ByteString.Lazy as LazyByteString
  import qualified Data.ByteString.Lazy.Char8 as Char8

  import Control.Monad (replicateM, when)

  -- Our custom parser type. We operate on lazy ByteStrings (which allows us to
  -- process it it in chunks, as well as only load parts of the '.class' file on
  -- demand). We also maintain a 'State' object.
  type Parser = Parsec ByteString ParserState

{-------------------------------------------------------------------------------
- TODO: Document for ClassFile Constants...
-------------------------------------------------------------------------------}

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

  parseConstants :: Parser [CPConstant]
  parseConstants = ((`subtract` 1) <$> getWord16i) >>= flip replicateM parseConstant

  -- The JVM Specification states that both CPLong and CPDouble are 'double width'
  -- in that they take up two entries instead of one. Hence, when we read one, we
  -- need to ensure that we skip an additional slot. To do this, we store whether
  -- we need to skip over an iteration as our user-state.
  parseConstant :: Parser CPConstant
  parseConstant = do

    -- Check if we need to skip and reset state if we do.
    _status <- getStatus
    if _status == skip then setStatus normal >> return CPDummy else do

    -- Check if we are processing double-width constant and mark that
    -- we need to skip next time.
    tag <- getWord8
    when (tag == 5 || tag == 6) $ setStatus skip

    -- Parse the actual constants based on their tag.
    case tag of
      7 -> CPClass <$> getWord16
      9 -> CPFieldref <$> getWord16 <*> getWord16
      10 -> CPMethodref <$> getWord16 <*> getWord16
      11 -> CPInterfaceMethodref <$> getWord16 <*> getWord16
      8 -> CPString <$> getWord16
      3 -> CPInteger <$> getWord32
      4 -> CPFloat <$> getWord32
      5 -> CPLong <$> getWord32 <*> getWord32
      6 -> CPDouble <$> getWord32 <*> getWord32
      12 -> CPNameAndType <$> getWord16 <*> getWord16
      1 -> CPUtf8 <$> (getWord16i >>= flip replicateM getWord8)
      15 -> CPMethodHandle <$> getWord8 <*> getWord16
      16 -> CPMethodType <$> getWord16
      18 -> CPInvokeDynamic <$> getWord16 <*> getWord16
      _ -> error ("Bad Constant Tag: " ++ show tag)

{-------------------------------------------------------------------------------
- TODO: Document for ClassFile Fields...
-------------------------------------------------------------------------------}

  -- Parsed representation of a '.class' file's fields.
  data Field = Field {
    accessFlags :: Word16,
    fieldNameIndex :: Word16,
    descriptorIndex :: Word16,
    attributes :: [Attribute]
  } deriving Show

  parseFields :: Parser [Field]
  parseFields = getWord16i >>= flip replicateM parseField

  parseField :: Parser Field
  parseField = Field <$> getWord16 <*> getWord16 <*> getWord16 <*> parseAttributes

{-------------------------------------------------------------------------------
- TODO: Document for ClassFile Methods...
-------------------------------------------------------------------------------}

  -- Parsed representation of a '.class' file's methods.
  data Method = Method {
    accessFlag :: Word16,
    methodNameIndex :: Word16,
    descriptorIndex :: Word16,
    attributes :: [Attribute]
  } deriving Show

  parseMethods :: Parser [Method]
  parseMethods = getWord16i >>= flip replicateM parseMethod

  parseMethod :: Parser Method
  parseMethod = Method <$> getWord16 <*> getWord16 <*> getWord16 <*> parseAttributes

{-------------------------------------------------------------------------------
- TODO: Document for ClassFile...
-------------------------------------------------------------------------------}

  -- Parsed representation of a '.class' file.
  data ClassFile = ClassFile {
    thisClassIdx :: Int,
    superClassIdx :: Int,
    constantPool :: [CPConstant],
    interfaces :: [Word16],
    fields :: [Field],
    methods :: [Method],
    attributes :: [Attribute]
  } deriving Show

  parseInterfaces :: Parser [Word16]
  parseInterfaces = getWord16i >>= flip replicateM getWord16

  -- Parses a '.class' file into it's runtime equivalent.
  parseClassFile :: String -> ByteString -> Either ParseError ClassFile
  parseClassFile = runParser parseClassFile' (normal, [])

  parseClassFile' :: Parser ClassFile
  parseClassFile' = do
    -- As we do not perform class file verification yet, we ignore the first
    -- 3 fields...
    _magic <- getWord32
    _minorVersion <- getWord16
    _majorVersion <- getWord16

    -- This is where the constant-pool should begin...
    _constantPool <- parseConstants

    -- Update state with constant pool...
    setConstantPool _constantPool

    -- These are currently ignored...
    _acessFlags <- getWord16
    _thisClass <- getWord16i
    _superClass <- getWord16i

    -- Now we have more dynamic fields: Interfaces, Fields, Methods, and Attributes.
    _interfaces <- parseInterfaces
    _fields <- parseFields
    _methods <- parseMethods
    _attributes <- parseAttributes

    -- Construct our class file
    return $ ClassFile _thisClass _superClass _constantPool _interfaces
      _fields _methods _attributes

{-------------------------------------------------------------------------------
- TODO: Document for ClassFile Attributes...
-------------------------------------------------------------------------------}

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

  parseAttributes :: Parser [Attribute]
  parseAttributes = getWord16i >>= flip replicateM parseAttribute

  parseAttribute :: Parser Attribute
  parseAttribute = do
    -- The next 16 bits is an index into the constant pool that holds a UTF8 constant.
    -- As well, each attribute can only be distinguished by its name, so we must
    -- retrieve it first to determine how to parse it next.
    name <- (Char8.unpack . LazyByteString.pack . utf8Bytes) <$> (getWord16 >>= getConstant)
    -- The next 32-bits determine the overall length of this attribute (not counting
    -- what we have found so far). This length is useful for when we not support
    -- the attribute (in order to safely skip over it), however when we do know
    -- the exact attribute, we may safely ignore it
    len <- getWord32i
    -- Now, we parse the rest of it based on the attribute name.
    case name of
      -- Constants are indexes into the constant pool. As these constants may vary
      -- in type (Int vs Double vs String vs etc.) we only know the actual type
      -- later.
      "ConstantValue" -> ConstantValue <$> getWord16
      -- Denotes the actual executable code. The code segment also holds both
      -- the maximum amount of operands and local variables alive at one time.
      "Code" -> CodeAttribute <$> getWord16 <*> getWord16 <*> parseCode
        <*> parseExceptionTable <*> parseAttributes
        where
          parseCode = getWord32i >>= flip replicateM getWord8
          parseExceptionTable = getWord16i >>= flip replicateM parseExceptionEntry
            where
              parseExceptionEntry = ExceptionEntry <$> getWord16 <*> getWord16
                <*> getWord16 <*> getWord16
      -- Not supported yet...
      _ -> UnknownAttribute name <$> replicateM len getWord8

  -- The state carried forward through parsing. Couples both the state and constant pool
  type ParserState = (Int, [CPConstant])

  -- Constant statuses...
  skip :: Int
  skip = 1

  normal :: Int
  normal = 0

  getConstantPool :: Parser [CPConstant]
  getConstantPool = snd <$> getState

  -- Update constant pool after it gets parsed, so it becomes accessible
  setConstantPool :: [CPConstant] -> Parser ()
  setConstantPool cp = getState >>= putState . BiFunctor.second (const cp)

  getStatus :: Parser Int
  getStatus = fst <$> getState

  -- Update status, but carry the constant pool forward.
  setStatus :: Int -> Parser ()
  setStatus s = getState >>= putState . BiFunctor.first (const s)

{-------------------------------------------------------------------------------
- TODO: Document for Parsing...
-------------------------------------------------------------------------------}
  noneOf :: String -> Parser Word8
  noneOf cs = satisfy (\b -> b `notElem` [c2w c | c <- cs])

  char :: Char -> Parser Word8
  char c = byte (c2w c)

  byte :: Word8 -> Parser Word8
  byte c = satisfy (==c)  <?> show [c]

  satisfy :: (Word8 -> Bool) -> Parser Word8
  satisfy f = tokenPrim toString getNextPos matchToken
    where
      toString tok = show [tok]
      getNextPos pos tok _ = updatePosChar pos tok
      matchToken tok = if f (c2w tok) then Just (c2w tok) else Nothing

  string :: String -> Parser ByteString
  string s = Char8.pack <$> tokens show updatePosString s

  {-
    Core Parse Functions.
  -}

  toBigEndian :: (Bits a, Num a) => ByteString -> a
  toBigEndian = LazyByteString.foldl' (\x y -> x `shiftL` 8 .|. fromIntegral y) 0

  readBytes :: (Integral a, Num b, Bits b) => a -> Parser b
  readBytes n = do
    val <- count (fromIntegral n) anyChar
    let valAsBytes = c2w `map` val in
      return $ toBigEndian . LazyByteString.pack $ valAsBytes


  getWord8 :: Parser Word8
  getWord8 = readBytes (1 :: Int)

  getWord8i :: (Integral a) => Parser a
  getWord8i = fromIntegral <$> getWord8

  getWord16 :: Parser Word16
  getWord16 = readBytes (2 :: Int)

  getWord16i :: (Integral a) => Parser a
  getWord16i = fromIntegral <$> getWord16

  getWord32 :: Parser Word32
  getWord32 = readBytes (4 :: Int)

  getWord32i :: (Integral a) => Parser a
  getWord32i = fromIntegral <$> getWord32

  getWord64 :: Parser Word64
  getWord64 = readBytes (8 :: Int)

  getWord64i :: (Integral a) => Parser a
  getWord64i = fromIntegral <$> getWord64

  getConstant :: (Integral a) => a -> Parser CPConstant
  getConstant idx = (!! fromIntegral idx) <$> getConstantPool
