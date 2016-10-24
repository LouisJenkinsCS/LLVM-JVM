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
    field_attributes_count :: Word16,
    field_attributes :: [Attribute_Info]
  }

  instance Show Field_Info where
    show (Field aflags nindex dindex acount attrs) = "Field: {access_flags:" ++
      show aflags ++ ",name_index:" ++ show nindex ++ ",descriptor_index:" ++
      show dindex ++ ",attribute_count:" ++ show acount ++ ",attributes:" ++ show attrs ++ "}\n"

  parseField :: [CP_Info] -> Parser Field_Info
  parseField cp = do
    aflags <- getNextShort
    nindex <- getNextShort
    dindex <- getNextShort
    acount <- getNextShort
    attrs <- replicateM (fromEnum acount) (parseAttribute cp)
    return $ Field aflags nindex dindex acount attrs
