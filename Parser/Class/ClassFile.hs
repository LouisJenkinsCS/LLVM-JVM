module Parser.Class.ClassFile where
  -- Imports for type declarations
  import Data.Word (Word16)
  import Data.ByteString.Lazy(ByteString)

  -- Imports for parsec
  import Text.Parsec(parse, ParseError)
  import Text.Parsec.ByteString.Lazy (Parser)

  -- Imports for helper methods
  import Parser.Class.Helpers (getWord16, getWord16i, getWord32)
  import Parser.Class.Constants (CPConstant, parseConstants)
  import Parser.Class.Fields (Field, parseFields)
  import Parser.Class.Methods (Method, parseMethods)
  import Parser.Class.Attributes(Attribute, parseAttributes)
  import Control.Monad (replicateM)

  {-
    Types
  -}

  -- A ClassFile is the runtime-equivalent to Java's '.class' files, and is parsed
  -- accordingly to the JVM Specification.
  data ClassFile = ClassFile {
    -- Constant Pool containts both the compile-time constants (such as strings,
    -- integers, and other literals) as well as class metadata such as a fields,
    -- methods, attributes, etc.
    constantPool :: [CPConstant],
    -- All interfaces this class inherits; these are indexes into constantPool
    -- to a CPClass constant.
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
  parseClassFile = parse parseClassFile'

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
