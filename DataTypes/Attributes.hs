module DataTypes.Attributes where
  import Data.Word
  import DataTypes.Parse_Bytes
  import Control.Monad.State.Lazy

  {-
    Attribute Types
  -}
  data Attribute_Info = Unknown_Attribute {
    attribute_name_index :: Word16,
    attribute_length :: Word32,
    attribute_info :: [Word8]
  } deriving (Show)

  parseAttribute :: Parser Attribute_Info
  parseAttribute = do
    nindex <- getNextShort
    len <- getNextInt
    info <- replicateM (fromEnum len) getNextByte
    return $ Unknown_Attribute nindex len info
