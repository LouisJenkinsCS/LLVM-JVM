module DataTypes.Methods where
  import DataTypes.Parse_Bytes
  import DataTypes.Attributes
  import Data.Word
  import Control.Monad.State
  import Data.Bits ((.&.))
  import DataTypes.Constant_Pool

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
    in "[" ++ foldr (\x y -> x ++ (if y /= "" then "," else "") ++ y) "" flags ++ "]"

  {-
    Method Type
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
      show dindex ++ ",attribute_count:" ++ show (length attrs) ++ ",attributes:" ++ show attrs ++ "}\n"



  parseMethods :: [CP_Info] -> Parser [Method_Info]
  parseMethods cp = getNextShort >>= \n -> replicateM (fromIntegral n) parseMethod
    where
      parseMethod = Method <$> getNextShort <*> getNextShort <*> getNextShort <*> parseAttributes cp
