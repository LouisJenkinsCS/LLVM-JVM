module Parser.Class.Attributes where
  -- Imports for type declarations
  import Data.Word (Word8, Word16)

  -- Imports for parsec
  import Text.Parsec.ByteString.Lazy(Parser)

  -- Imports for helper methods
  import Parser.Class.Helpers (getWord8, getWord16, getWord16i, getWord32i)
  import Control.Monad (replicateM)

  {-
    Types
  -}

  -- Parser Representation of a '.class' file's attributes.
  data Attribute = Attribute {
    attributeNameIndex :: Word16,
    info :: [Word8]
  } deriving Show

  {-
    Parse functions
  -}

  parseAttributes :: Parser [Attribute]
  parseAttributes = getWord16i >>= flip replicateM parseAttribute

  parseAttribute :: Parser Attribute
  parseAttribute = Attribute <$> getWord16 <*> (getWord32i >>= flip replicateM getWord8)
