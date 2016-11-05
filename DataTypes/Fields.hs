module DataTypes.Fields where
  import Data.Word
  import DataTypes.Attributes
  import DataTypes.Parse_Bytes
  import Control.Monad.State
  import DataTypes.Constant_Pool

  {-
    Field Type
  -}
  data Field_Info = Field {
    field_access_flags :: Word16,
    field_name_index :: Word16,
    field_descriptor_index :: Word16,
    field_attributes :: [Attribute_Info]
  }

  instance Show Field_Info where
    show (Field aflags nindex dindex attrs) = "Field: {access_flags:" ++
      show aflags ++ ",name_index:" ++ show nindex ++ ",descriptor_index:" ++
      show dindex ++ ",attribute_count:" ++ show (length attrs) ++ ",attributes:" ++ show attrs ++ "}\n"

  parseFields :: [CP_Info] -> Parser [Field_Info]
  parseFields cp = getNextShort >>= \n -> replicateM (fromIntegral n) parseField
    where
      parseField = Field <$> getNextShort <*> getNextShort <*> getNextShort <*> parseAttributes cp
