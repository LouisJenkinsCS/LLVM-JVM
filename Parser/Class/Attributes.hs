module Parser.Class.Attributes where
  import Parser.Class.Types
  
  -- Imports for helper methods
  import Parser.Class.Helpers (getWord8, getWord16, getWord16i, getWord32i)
  import Control.Monad (replicateM)

  {-
    TODO:

      Need to maintain a mapping of (Name -> Info), so we can easily identify an
      attribute by its name alone (as looking at it, that is how it is commonly)
      used anyway...

      Likely, instead of having Attributes be '[Attribute]', we have 'Map String [Word8]',
      or better yet have concrete data constructors so we have 'Map String Attribute'.
      This will make it 10x easier and faster to retrieve information at runtime and
      easier to use.
  -}


  {-
    Parse functions
  -}

  parseAttributes :: Parser [Attribute]
  parseAttributes = getWord16i >>= flip replicateM parseAttribute

  parseAttribute :: Parser Attribute
  parseAttribute = Attribute <$> getWord16 <*> (getWord32i >>= flip replicateM getWord8)
