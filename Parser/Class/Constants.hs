module Parser.Class.Constants where
  -- Imports for type declarations
  import Data.Word (Word8, Word16, Word32)

  -- Imports for parsec
  import Text.Parsec ((<?>), (<|>))
  import Text.Parsec.ByteString

  -- Imports for helper methods
  import Parser.Class.Helpers (getWord8, getWord16, getWord32, getWord64)



  {-
    Types for Constant Pool
  -}

  data CPConstant =
    -- Class
    CPClass {
      nameIndex :: Word16
    } |
    CPFieldref {
      classIndex :: Word16,
      nameAndTypeIndex :: Word16
    } |
    CPMethodref {
      classIndex :: Word16,
      nameAndTypeIndex :: Word16
    } |
    CPInterfaceMethodref {
      classIndex :: Word16,
      nameAndTypeIndex :: Word16
    } |
    CPString {
      stringIndex :: Word16
    } |
    CPInteger {
      bytes :: Word32
    } |
    CPFloat {
      bytes :: Word32
    } |
    CPLong {
      highBytes :: Word32,
      lowBytes :: Word32
    } |
    CPDouble {
      highBytes :: Word32,
      lowBytes :: Word32
    } |
    CPNameAndType {
      nameIndex :: Word16,
      descriptorIndex :: Word16
    } |
    CPUtf8 {
      utf8Bytes :: [Word16]
    } |
    CPMethodHandle {
      referenceKind :: Word8,
      referenceIndex :: Word16
    } |
    CPMethodType {
      descriptorIndex :: Word16
    } |
    CPInvokeDynamic {
      bootstrapMethodAttrIndex :: Word16,
      nameAndTypeIndex :: Word16
    }

  -- Custom 'showable' for better debugging
  instance Show CPConstant where
    show (CPClass _nameIndex) =
      "CONSTANT_Class{" ++
        "name_index=" ++ show _nameIndex ++
      "}"

    show (CPFieldref _classIndex _nameAndTypeIndex) =
      "CONSTANT_Fieldref{" ++
        "class_index=" ++ show _classIndex ++
        ",name_and_type_index=" ++ show _nameAndTypeIndex ++
      "}"

    show (CPMethodref _classIndex _nameAndTypeIndex) =
      "CONSTANT_Methodref{" ++
        "class_index=" ++ show _classIndex ++
        ",name_and_type_index=" ++ show _nameAndTypeIndex ++
      "}"

    show (CPInterfaceMethodref _classIndex _nameAndTypeIndex) =
      "CONSTANT_InterfaceMethodref{" ++
        "class_index=" ++ show _classIndex ++
        ",name_and_type_index=" ++ show _nameAndTypeIndex ++
      "}"

    show (CPString _stringIndex) =
      "CONSTANT_String{" ++
        "string_index=" ++ show _stringIndex ++
      "}"

    show (CPInteger _bytes) =
      "CONSTANT_Integer{" ++
        "bytes=" ++ show _bytes ++
      "}"

    show (CPFloat _bytes) =
      "CONSTANT_Float{" ++
        "bytes=" ++ show _bytes ++
      "}"

    show (CPLong _highBytes _lowBytes) =
      "CONSTANT_Long{" ++
        "high_bytes=" ++ show _highBytes ++
        ",low_bytes=" ++ show _lowBytes ++
      "}"

    show (CPDouble _highBytes _lowBytes) =
      "CONSTANT_Double{" ++
        "high_bytes=" ++ show _highBytes ++
        ",low_bytes=" ++ show _lowBytes ++
      "}"

    show (CPNameAndType _nameAndTypeIndex _descriptorIndex) =
      "CONSTANT_NameAndType{" ++
        "name_and_type_index=" ++ show _nameAndTypeIndex ++
        ",descriptor_index=" ++ show _descriptorIndex ++
      "}"

    show (CPUtf8 _utf8Bytes) =
      "CONSTANT_Utf8{" ++
        "bytes[" ++ (show . length $ _utf8Bytes) ++ "]=" ++ show _utf8Bytes ++
      "}"

    show (CPMethodHandle _referenceKind _referenceIndex) =
      "CONSTANT_MethodHandle{" ++
        "reference_kind=" ++ show _referenceKind ++
        ",reference_index=" ++ show _referenceIndex ++
      "}"

    show (CPMethodType _descriptorIndex) =
      "CONSTANT_MethodType{" ++
        "descriptor_index=" ++ show _descriptorIndex ++
      "}"

    show (CPInvokeDynamic _bootstrapMethodAttrIndex _nameAndTypeIndex) =
      "CONSTANT_InvokeDynamic{" ++
        "bootstrap_method_attr_index=" ++ show _bootstrapMethodAttrIndex ++
        ",name_and_type_index=" ++ show _nameAndTypeIndex ++
      "}"


  -- The Constant Pool consists of a 16-bit count (denoting the amount of constants
  -- this class file holds) followed by `cp_info` structures, the constants themselves.
  -- Note: All indexes are 0-based.
  -- parseConstants :: Parser [CPConstant]
  -- parseConstants = do
  --   cpCount <- getWord16
  --   return
