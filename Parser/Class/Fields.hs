module Parser.Class.Fields where
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

  -- Parsed representation of a '.class' file's fields.
  data Field = Field {
    accessFlags :: Word16,
    nameIndex :: Word16,
    descriptorIndex :: Word16,
    attributes :: [Attribute]
  } deriving Show

  {-
    Parse functions
  -}

  parseFields :: Parser [Field]
  parseFields = getWord16i >>= flip replicateM parseField

  parseField :: Parser Field
  parseField = Field <$> getWord16 <*> getWord16 <*> getWord16 <*> parseAttributes
