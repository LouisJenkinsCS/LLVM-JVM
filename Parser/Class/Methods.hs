module Parser.Class.Methods where
  -- Imports for type declarations
  import Data.Word (Word16)

  -- Imports for parsec
  import Text.Parsec.ByteString.Lazy(Parser)

  -- Imports for helper methods
  import Parser.Class.Helpers (getWord16, getWord16i)
  import Parser.Class.Attributes (Attribute, parseAttributes)
  import Control.Monad (replicateM)

  {-
    Types
  -}

  -- Parsed representation of a '.class' file's methods.
  data Method = Method {
    accessFlag :: Word16,
    nameIndex :: Word16,
    descriptorIndex :: Word16,
    attributes :: [Attribute]
  } deriving Show

  {-
    Parser Functions
  -}

  parseMethods :: Parser [Method]
  parseMethods = getWord16i >>= flip replicateM parseMethod

  parseMethod :: Parser Method
  parseMethod = Method <$> getWord16 <*> getWord16 <*> getWord16 <*> parseAttributes
