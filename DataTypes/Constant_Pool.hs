module DataTypes.Constant_Pool where
  import Data.Word
  import Data.ByteString (pack)
  import Data.ByteString.Char8 (unpack)
  import DataTypes.Parse_Bytes
  import Control.Monad

  newtype UTF8_Bytes = UTF8_Bytes [Word8]

  instance Show UTF8_Bytes where
     show (UTF8_Bytes b) = Data.ByteString.Char8.unpack $ pack b
  {-
    Constant Pool types
  -}
  data CP_Info = Class_Info {
    tag :: Word8,
    name_index :: Word16
  } | Fieldref_Info {
    tag :: Word8,
    class_index :: Word16,
    name_and_type_index :: Word16
  } | Methodref_Info {
    tag :: Word8,
    class_index :: Word16,
    name_and_type_index :: Word16
  } | InterfaceMethodref_Info {
    tag :: Word8,
    class_index :: Word16,
    name_and_type_index :: Word16
  } | String_Info {
    tag :: Word8,
    string_index :: Word16
  } | Integer_Info {
    tag :: Word8,
    bytes :: Word32
  } | Float_Info {
    tag :: Word8,
    bytes :: Word32
  } | Long_Info {
    tag :: Word8,
    high_bytes :: Word32,
    low_bytes :: Word32
  } | Double_Info {
    tag :: Word8,
    high_bytes :: Word32,
    low_bytes :: Word32
  } | NameAndType_Info {
    tag :: Word8,
    name_index :: Word16,
    descriptor_index :: Word16
  } | Utf8_Info {
    tag :: Word8,
    utf8_bytes :: UTF8_Bytes
  } | MethodHandle_Info {
    tag :: Word8,
    reference_kind :: Word8,
    reference_index :: Word16
  } | MethodType_Info {
    tag :: Word8,
    descriptor_index :: Word16
  } | InvokeDynamic_Info {
    tag :: Word8,
    bootstrap_method_attr_index :: Word16,
    name_and_type_index :: Word16
  } | Dummy_Info {

  }

  instance Show CP_Info where
    show (Class_Info t n) = printTag t ++ "{name_index:" ++ show n ++ "}\n"
    show (Fieldref_Info t c n) = printTag t ++ "{class_index:" ++ show c ++ ",name_and_type_index:" ++ show n ++ "}\n"
    show (Methodref_Info t c n) = printTag t ++ "{class_index:" ++ show c ++ ",name_and_type_index:" ++ show n ++ "}\n"
    show (InterfaceMethodref_Info t c n) = printTag t ++ "{class_index:" ++ show c ++ ",name_and_type_index:" ++ show n ++ "}\n"
    show (String_Info t n) = printTag t ++ "{string_index:" ++ show n ++ "}\n"
    show (Integer_Info t b) = printTag t ++ "{bytes:" ++ show b ++ "}\n"
    show (Float_Info t b) = printTag t ++ "{bytes:" ++ show b ++ "}\n"
    show (Long_Info t hb lb) = printTag t ++ "{high_bytes:" ++ show hb ++ ",low_bytes:" ++ show lb ++ "}\n"
    show (Double_Info t hb lb) = printTag t ++ "{high_bytes:" ++ show hb ++ ",low_bytes:" ++ show lb ++ "}\n"
    show (NameAndType_Info t n d) = printTag t ++ "{name_index:" ++ show n ++ ",descriptor_index:" ++ show d ++ "}\n"
    show (Utf8_Info t b) = printTag t ++ "{bytes:" ++ show b ++ "}\n"
    show (MethodHandle_Info t rk ri) = printTag t ++ "{reference_kind:" ++ show rk ++ ",reference_index:" ++ show ri ++ "}\n"
    show (MethodType_Info t d) = printTag t ++ "{descriptor_index:" ++ show d ++ "}\n"
    show (InvokeDynamic_Info t bm n) = printTag t ++ "{bootstrap_method_attr_index:" ++ show bm ++ ",name_and_type_index:" ++ show n ++"}\n"
    show Dummy_Info = "\n"


  {-
    Parses all constants in the Constant Pool. The first 2 bytes contain the size of the
    constant pool. The JVM specification states that the constant pool is indexed from
    [1, cp_count), and so index 0 is filled in with a dummy constant.
  -}
  parseConstants :: Parser [CP_Info]
  parseConstants = getNextShort >>= \n -> replicateM (fromIntegral n - 1) parseConstant
    >>= \cp -> return $ Dummy_Info : cp
      where
        parseConstant = do
          t <- getNextByte
          case t of
            7 -> Class_Info t <$> getNextShort
            9 -> Fieldref_Info t <$> getNextShort <*> getNextShort
            10 -> Methodref_Info t <$> getNextShort <*> getNextShort
            11 -> InterfaceMethodref_Info t <$> getNextShort <*> getNextShort
            8 -> String_Info t <$> getNextShort
            3 -> Integer_Info t <$> getNextInt
            4 -> Float_Info t <$> getNextInt
            5 -> Long_Info t <$> getNextInt <*> getNextInt
            6 -> Double_Info t <$> getNextInt <*> getNextInt
            12 -> NameAndType_Info t <$> getNextShort <*> getNextShort
            1 -> Utf8_Info t <$> parseUTF8
              where
                parseUTF8 = getNextShort >>= \n -> UTF8_Bytes <$> getNextBytes (fromIntegral n)
            15 -> MethodHandle_Info t <$> getNextByte <*> getNextShort
            16 -> MethodType_Info t <$> getNextShort
            18 -> InvokeDynamic_Info t <$> getNextShort <*> getNextShort
            _ -> undefined

  {-
    Stringify the tag for a Constant Pool type.
  -}
  printTag :: Word8 -> String
  printTag t = "CONSTANT_" ++ case t of
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
