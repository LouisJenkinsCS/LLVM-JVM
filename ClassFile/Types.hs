module ClassFile.Types where
  import Data.Word
  import Data.Bits
  import Control.Monad.State
  import Data.ByteString.Char8
  import Data.ByteString

  {-
    Parser
  -}
  type Parser = State ByteString



  {-
    Attribute
  -}


  data Exception_Table = Exception_Table {
    start_pc :: Word16,
    end_pc :: Word16,
    handler_pc :: Word16,
    catch_type :: Word16
  } deriving (Show)

  -- Attribute Types
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



  {-
    Class_File
  -}

  -- Data structure representation of a `.class` file.
  data ClassFile = ClassFile {
      cp_info :: [CP_Info],
      access_flags :: Word16,
      this_class :: Word16,
      super_class :: Word16,
      interfaces :: [Word16],
      fields :: [Field_Info],
      methods :: [Method_Info],
      classfile_attributes :: [Attribute_Info]
  } deriving (Show)



  {-
    Constant Pool
  -}

  -- Wrapper for Utf8_info constant-pool type.
  newtype UTF8_Bytes = UTF8_Bytes [Word8]

  instance Show UTF8_Bytes where
     show (UTF8_Bytes b) = Data.ByteString.Char8.unpack $ Data.ByteString.pack b



  -- Constant Pool types
  data CP_Info = Class_Info { tag :: Word8, name_index :: Word16 }
    | Fieldref_Info { tag :: Word8, class_index :: Word16, name_and_type_index :: Word16 }
    | Methodref_Info { tag :: Word8, class_index :: Word16, name_and_type_index :: Word16 }
    | InterfaceMethodref_Info { tag :: Word8, class_index :: Word16, name_and_type_index :: Word16 }
    | String_Info { tag :: Word8, string_index :: Word16}
    | Integer_Info { tag :: Word8, bytes :: Word32 }
    | Float_Info { tag :: Word8, bytes :: Word32 }
    | Long_Info { tag :: Word8, high_bytes :: Word32, low_bytes :: Word32 }
    | Double_Info { tag :: Word8, high_bytes :: Word32, low_bytes :: Word32 }
    | NameAndType_Info { tag :: Word8, name_index :: Word16, descriptor_index :: Word16 }
    | Utf8_Info { tag :: Word8, utf8_bytes :: UTF8_Bytes }
    | MethodHandle_Info { tag :: Word8, reference_kind :: Word8, reference_index :: Word16 }
    | MethodType_Info { tag :: Word8, descriptor_index :: Word16 }
    | InvokeDynamic_Info { tag :: Word8, bootstrap_method_attr_index :: Word16, name_and_type_index :: Word16 }
    | Dummy_Info

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


  {-
    Method
  -}

  data Method_Info = Method {
    method_access_flags :: Word16,
    method_name_index :: Word16,
    method_descriptor_index :: Word16,
    method_attributes :: [Attribute_Info]
  }

  instance Show Method_Info where
    show (Method aflags nindex dindex attrs) = "Method: {access_flags:" ++
      showAccFlags aflags ++ ",name_index:" ++ show nindex ++ ",descriptor_index:" ++
      show dindex ++ ",attribute_count:" ++ show (Prelude.length attrs) ++ ",attributes:" ++ show attrs ++ "}\n"

  showAccFlags :: Word16 -> String
  showAccFlags n = let
    public = ["ACC_PUBLIC" | n .&. 0x1 /= 0]
    private = ["ACC_PRIVATE" | n .&. 0x2 /= 0]
    protected = ["ACC_PROTECTED" | n .&. 0x4 /= 0]
    static = ["ACC_STATIC" | n .&. 0x8 /= 0]
    final = ["ACC_FINAL" | n .&. 0x10 /= 0]
    sync = ["ACC_SYNCHRONIZED" | n .&. 0x20 /= 0]
    bridge = ["ACC_BRIDGE" | n .&. 0x40 /= 0]
    varargs = ["ACC_VARARGS" | n .&. 0x80 /= 0]
    native = ["ACC_NATIVE" | n .&. 0x100 /= 0]
    abstract = ["ACC_ABSTRACT" | n .&. 0x400 /= 0]
    strict = ["ACC_STRICT" | n .&. 0x800 /= 0]
    synth = ["ACC_SYNTHETIC" | n .&. 0x1000 /= 0]
    flags = public ++ private ++ protected ++ static ++ final ++ sync ++
      bridge ++ varargs ++ native ++ abstract ++ strict ++ synth
    in "[" ++ Prelude.foldr (\x y -> x ++ (if y /= "" then "," else "") ++ y) "" flags ++ "]"

  {-
    Field
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
      show dindex ++ ",attribute_count:" ++ show (Prelude.length attrs) ++ ",attributes:" ++ show attrs ++ "}\n"
