module DataTypes.Attributes where
  import Data.Word
  import DataTypes.Parse_Bytes
  import Control.Monad.State.Lazy
  import DataTypes.Constant_Pool

  data Exception_Table = Exception_Table {
    start_pc :: Word16,
    end_pc :: Word16,
    handler_pc :: Word16,
    catch_type :: Word16
  } deriving (Show)

  -- parseExceptionTable :: Parser Exception_Table
  -- parseExceptionTable = Exception_Table <$> getNextShort <*> getNextShort <*> getNextShort <*> getNextShort

  {-
    Attribute Types
  -}
  data Attribute_Info = Unknown_Attribute {
    attribute_name_index :: Word16,
    attribute_length :: Word32,
    attribute_info :: [Word8],
    attribute_name :: String
  } | ConstantValue_Attribute {
    attribute_name_index :: Word16,
    attribute_length :: Word32,
    constantvalue_index :: Word16
  } | Code_Attribute {
    attribute_name_index :: Word16,
    attribute_length :: Word32,
    max_stack :: Word16,
    max_locals :: Word16,
    code :: [Word8],
    exception_table :: [Exception_Table],
    attributes :: [Attribute_Info]
  } | StackMapTable_Attribute {
  } | Exceptions_Attribute {
  } | BootstrapMethods_Attribute {
  } | InnerClasses_Attribute {
  } | EnclosingMethod_Attribute | Synthetic_Attribute | Signature_Attribute |
    RuntimeVisibleAnnotations_Attribute | RuntimeInvisibleAnnotations_Attribute |
    RuntimeVisibleParameterAnnotations_Attribute | RuntimeInvisibleParameterAnnotations_Attribute |
    RuntimeVisibleTypeAnnotations_Attribute | RuntimeInvisibleTypeAnnotations_Attribute |
    AnnotationDefault_Attribute | MethodParameters_Attribute deriving (Show)

  parseAttributes :: [CP_Info] -> Parser [Attribute_Info]
  parseAttributes cp = getNextShort >>= \n -> replicateM (fromIntegral n) parseAttribute
    where
      parseAttribute = do
        nindex <- getNextShort
        len <- getNextInt
        let name = show $ utf8_bytes $ cp !! fromIntegral nindex
        case name of
          "ConstantValue" -> ConstantValue_Attribute nindex len <$> getNextShort
          "Code" -> Code_Attribute nindex len <$> getNextShort <*> getNextShort <*> parseCode
            <*> parseExceptionTables <*> parseAttributes cp
              where
                parseExceptionTables = getNextShort >>= \n -> replicateM (fromIntegral n) parseExceptionTable
                  where
                    parseExceptionTable = Exception_Table <$> getNextShort <*> getNextShort <*> getNextShort <*> getNextShort
                parseCode = getNextInt >>= getNextBytes . fromIntegral
          _ -> Unknown_Attribute nindex len <$> getNextBytes (fromIntegral len) <*> return name

  getCodeAttribute :: [CP_Info] -> [Attribute_Info] -> Attribute_Info
  getCodeAttribute = findCodeAttribute
    where
      findCodeAttribute :: [CP_Info] -> [Attribute_Info] -> Attribute_Info
      findCodeAttribute cp (x:xs) = let
        nindex = attribute_name_index x
        name = show $ utf8_bytes $ cp !! fromEnum nindex
        in
          if name == "Code" then x else findCodeAttribute cp xs
      findCodeAttribute _ [] = undefined
