module Parser.Class.ClassFile where
  -- Imports for type declarations
  import Data.Word (Word8, Word16, Word32)
  import Data.ByteString.Lazy(ByteString)

  -- Imports for parsec
  import Text.Parsec(parse, ParseError)
  import Text.Parsec.ByteString.Lazy (Parser)

  -- Imports for helper methods
  import Parser.Class.Helpers (getWord8, getWord16, getWord32)
  import Parser.Class.Constants (CPConstant, parseConstants)
  import Control.Monad (replicateM)
  import Misc.Logger

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
    interfaces :: [Word16]
  }

  -- Parses a '.class' file into it's runtime equivalent.
  parseClassFile :: String -> ByteString -> Either ParseError ClassFile
  parseClassFile = parse parseClassFile'

  parseClassFile' :: Parser ClassFile
  parseClassFile' = do
    -- As we do not perform class file verification yet, we ignore the first
    -- 3 fields...
    magic <- getWord32
    minorVersion <- getWord16
    majorVersion <- getWord16

    -- This is where the constant-pool should begin...
    constantPool <- parseConstants

    -- These are currently ignored...
    acessFlags <- getWord16
    thisClass <- getWord16
    superClass <- getWord16

    -- Now we have more dynamic fields: Interfaces, Fields, Methods, and Attributes.
    interfaces <- parseInterfaces
    fields <- parseFields
    methods <- parseMethods
    attributes <- parseAttributes

    -- Construct our class file
    return $ ClassFile constantPool interfaces


  parseInterfaces :: Parser [Word16]
  parseInterfaces = (fromIntegral <$> getWord16) >>= flip replicateM getWord16
