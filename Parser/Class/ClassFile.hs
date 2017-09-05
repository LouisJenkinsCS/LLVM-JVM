module Parser.Class.ClassFile where
  -- Imports for type declarations
  import Data.Word (Word16)
  import Data.ByteString.Lazy(ByteString)

  -- Imports for parsec
  import Text.Parsec(runParser, ParseError)

  -- Imports for helper methods
  import Parser.Class.Helpers (Parser, getWord16, getWord16i, getWord32)
  import Parser.Class.Constants (CPConstant, parseConstants)
  import Parser.Class.Fields (Field, parseFields)
  import Parser.Class.Methods (Method, parseMethods)
  import Parser.Class.Attributes(Attribute, parseAttributes)
  import Control.Monad (replicateM)

  {-
    TODO

      Need to add more helper methods, one for every conceiveable use-case. Some
      examples could be:
  -}

  -- Parsed representation of a '.class' file.
  data ClassFile = ClassFile {
    constantPool :: [CPConstant],
    interfaces :: [Word16],
    fields :: [Field],
    methods :: [Method],
    attributes :: [Attribute]
  } deriving Show

  {-
    Parse Functions
  -}

  parseInterfaces :: Parser [Word16]
  parseInterfaces = getWord16i >>= flip replicateM getWord16

  -- Parses a '.class' file into it's runtime equivalent.
  parseClassFile :: String -> ByteString -> Either ParseError ClassFile
  parseClassFile = runParser parseClassFile' False

  parseClassFile' :: Parser ClassFile
  parseClassFile' = do
    -- As we do not perform class file verification yet, we ignore the first
    -- 3 fields...
    _magic <- getWord32
    _minorVersion <- getWord16
    _majorVersion <- getWord16

    -- This is where the constant-pool should begin...
    _constantPool <- parseConstants

    -- These are currently ignored...
    _acessFlags <- getWord16
    _thisClass <- getWord16
    _superClass <- getWord16

    -- Now we have more dynamic fields: Interfaces, Fields, Methods, and Attributes.
    _interfaces <- parseInterfaces
    _fields <- parseFields
    _methods <- parseMethods
    _attributes <- parseAttributes

    -- Construct our class file
    return $ ClassFile _constantPool _interfaces _fields _methods _attributes


  -- Get an interface we implement by index.
  getInterfaceByIndex :: (Integral a) => a -> ClassFile
  getInterfaceByIndex = undefined

  -- Get an interface we implement by name
  getInterfaceByName :: String -> ClassFile
  getInterfaceByName = undefined

  -- Get a field by name
  getFieldByName :: String -> Field
  getFieldByName = undefined

  -- Get a field by index
  getFieldByIndex :: (Integral a) => a -> Field
  getFieldByIndex = undefined

  -- Get a method by name
  getMethodByName :: String -> Method
  getMethodByName = undefined

  -- Get a method by index
  getMethodByIndex :: (Integral a) => a -> Method
  getMethodByIndex = undefined
