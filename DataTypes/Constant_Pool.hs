module DataTypes.Constant_Pool where
  import Data.Word
  import Data.ByteString
  import Data.ByteString.Char8 (unpack)
  import DataTypes.Parse_Bytes

  newtype UTF8_Bytes = UTF8_Bytes [Word8]
  instance Show UTF8_Bytes where
    show (UTF8_Bytes b) = Data.ByteString.Char8.unpack $ pack b

  newtype CP_Tag = CP_Tag Word8
  instance Show CP_Tag where
    show = printTag

  {-
    Constant Pool types
  -}
  data CP_Info = Class_Info {
    tag :: CP_Tag,
    name_index :: Word16
  } | Fieldref_Info {
    tag :: CP_Tag,
    class_index :: Word16,
    name_and_type_index :: Word16
  } | Methodref_Info {
    tag :: CP_Tag,
    class_index :: Word16,
    name_and_type_index :: Word16
  } | InterfaceMethodref_Info {
    tag :: CP_Tag,
    class_index :: Word16,
    name_and_type_index :: Word16
  } | String_Info {
    tag :: CP_Tag,
    string_index :: Word16
  } | Integer_Info {
    tag :: CP_Tag,
    bytes :: Word32
  } | Float_Info {
    tag :: CP_Tag,
    bytes :: Word32
  } | Long_Info {
    tag :: CP_Tag,
    high_bytes :: Word32,
    low_bytes :: Word32
  } | Double_Info {
    tag :: CP_Tag,
    high_bytes :: Word32,
    low_bytes :: Word32
  } | NameAndType_Info {
    tag :: CP_Tag,
    name_index :: Word16,
    descriptor_index :: Word16
  } | Utf8_Info {
    tag :: CP_Tag,
    length :: Word16,
    utf8_bytes :: UTF8_Bytes
  } | MethodHandle_Info {
    tag :: CP_Tag,
    reference_kind :: Word8,
    reference_index :: Word16
  } | MethodType_Info {
    tag :: CP_Tag,
    descriptor_index :: Word16
  } | InvokeDynamic_Info {
    tag :: CP_Tag,
    bootstrap_method_attr_index :: Word16,
    name_and_type_index :: Word16
  } | Dummy_Info {

  }

  instance Show CP_Info where
    show (Class_Info t n) = show t ++ "{name_index:" ++ show n ++ "}\n"
    show (Fieldref_Info t c n) = show t ++ "{class_index:" ++ show c ++ ",name_and_type_index:" ++ show n ++ "}\n"
    show (Methodref_Info t c n) = show t ++ "{class_index:" ++ show c ++ ",name_and_type_index:" ++ show n ++ "}\n"
    show (InterfaceMethodref_Info t c n) = show t ++ "{class_index:" ++ show c ++ ",name_and_type_index:" ++ show n ++ "}\n"
    show (String_Info t n) = show t ++ "{string_index:" ++ show n ++ "}\n"
    show (Integer_Info t b) = show t ++ "{bytes:" ++ show b ++ "}\n"
    show (Float_Info t b) = show t ++ "{bytes:" ++ show b ++ "}\n"
    show (Long_Info t hb lb) = show t ++ "{high_bytes:" ++ show hb ++ ",low_bytes:" ++ show lb ++ "}\n"
    show (Double_Info t hb lb) = show t ++ "{high_bytes:" ++ show hb ++ ",low_bytes:" ++ show lb ++ "}\n"
    show (NameAndType_Info t n d) = show t ++ "{name_index:" ++ show n ++ ",descriptor_index:" ++ show d ++ "}\n"
    show (Utf8_Info t l b) = show t ++ "{length:" ++ show l ++ ",bytes:" ++ show b ++ "}\n"
    show (MethodHandle_Info t rk ri) = show t ++ "{reference_kind:" ++ show rk ++ ",reference_index:" ++ show ri ++ "}\n"
    show (MethodType_Info t d) = show t ++ "{descriptor_index:" ++ show d ++ "}\n"
    show (InvokeDynamic_Info t bm n) = show t ++ "{bootstrap_method_attr_index:" ++ show bm ++ ",name_and_type_index:" ++ show n ++"}\n"
    show Dummy_Info = "\n"

  parseConstant :: Parser CP_Info
  parseConstant = do
    t <- getNextByte
    case t of
      7 -> do
        nindex <- getNextShort
        return $ Class_Info (CP_Tag t) nindex
      9 -> do
        cindex <- getNextShort
        ntindex <- getNextShort
        return $ Fieldref_Info (CP_Tag t) cindex ntindex
      10 -> do
        cindex <- getNextShort
        ntindex <- getNextShort
        return $ Methodref_Info (CP_Tag t) cindex ntindex
      11 -> do
        cindex <- getNextShort
        ntindex <- getNextShort
        return $ InterfaceMethodref_Info (CP_Tag t) cindex ntindex
      8 -> do
        sindex <- getNextShort
        return $ String_Info (CP_Tag t) sindex
      3 -> do
        b <- getNextInt
        return $ Integer_Info (CP_Tag t) b
      4 -> do
        b <- getNextInt
        return $ Float_Info (CP_Tag t) b
      5 -> do
        hb <- getNextInt
        lb <- getNextInt
        return $ Long_Info (CP_Tag t) hb lb
      6 -> do
        hb <- getNextInt
        lb <- getNextInt
        return $ Double_Info (CP_Tag t) hb lb
      12 -> do
        nindex <- getNextShort
        dindex <- getNextShort
        return $ NameAndType_Info (CP_Tag t) nindex dindex
      1 -> do
        len <- getNextShort
        b <- getNextBytes (fromEnum len)
        return $ Utf8_Info (CP_Tag t) len (UTF8_Bytes b)
      15 -> do
        rkind <- getNextByte
        rindex <- getNextShort
        return $ MethodHandle_Info (CP_Tag t) rkind rindex
      16 -> do
        dindex <- getNextShort
        return $ MethodType_Info (CP_Tag t) dindex
      18 -> do
        bmaindex <- getNextShort
        ntindex <- getNextShort
        return $ InvokeDynamic_Info (CP_Tag t) bmaindex ntindex
      _ -> undefined

  {-
    Stringify the tag for a Constant Pool type.
  -}
  printTag :: CP_Tag -> String
  printTag (CP_Tag t) = "CONSTANT_" ++ case t of
    7 -> "Class"
    9 -> "Fieldref"
    10 -> "Methodref"
    11 -> "InterfaceMethodref"
    8 -> "String"
    3 -> "Integer"
    4 -> "Float"
    5 -> "Long"
    6 -> "Double"
    12 -> "NameAndType"
    1 -> "Utf8"
    15 -> "MethodHandle"
    16 -> "MethodType"
    18 -> "InvokeDynamic"
    _ -> "Unknown"
