module Parser.Class.Methods where
  import Parser.Class.Types

  -- Imports for helper methods
  import Parser.Class.Helpers (getWord16, getWord16i)
  import Parser.Class.Attributes (parseAttributes)
  import Control.Monad (replicateM)


  {-
    Parser Functions
  -}

  parseMethods :: Parser [Method]
  parseMethods = getWord16i >>= flip replicateM parseMethod

  parseMethod :: Parser Method
  parseMethod = Method <$> getWord16 <*> getWord16 <*> getWord16 <*> parseAttributes
