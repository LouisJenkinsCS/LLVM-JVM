module Parser.Class.Fields where
  import Parser.Class.Types

  -- Imports for helper methods
  import Parser.Class.Helpers (getWord16, getWord16i)
  import Parser.Class.Attributes (parseAttributes)
  import Control.Monad (replicateM)

  {-
    Parse functions
  -}

  parseFields :: Parser [Field]
  parseFields = getWord16i >>= flip replicateM parseField

  parseField :: Parser Field
  parseField = Field <$> getWord16 <*> getWord16 <*> getWord16 <*> parseAttributes
