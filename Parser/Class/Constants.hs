module Parser.Class.Constants where
  -- Imports for type declarations
  import Data.Word (Word8, Word16, Word32)

  -- Imports for parsec
  import Text.Parsec.ByteString.Lazy(Parser)

  -- Imports for helper methods
  import Parser.Class.Helpers (getWord8, getWord16, getWord16i, getWord32)
  import Control.Monad (replicateM)

  {-
    Types for Constant Pool
  -}

  -- Parsed representation of a '.class' file's constant-pool constants.
  data CPConstant = CPClass { nameIndex :: Word16 }
    | CPFieldref { classIndex :: Word16, nameAndTypeIndex :: Word16 }
    | CPMethodref { classIndex :: Word16, nameAndTypeIndex :: Word16 }
    | CPInterfaceMethodref { classIndex :: Word16, nameAndTypeIndex :: Word16 }
    | CPString { stringIndex :: Word16 }
    | CPInteger { bytes :: Word32 }
    | CPFloat { bytes :: Word32 }
    | CPLong { highBytes :: Word32, lowBytes :: Word32 }
    | CPDouble { highBytes :: Word32, lowBytes :: Word32 }
    | CPNameAndType { nameIndex :: Word16, descriptorIndex :: Word16 }
    | CPUtf8 { utf8Bytes :: [Word8] }
    | CPMethodHandle { referenceKind :: Word8, referenceIndex :: Word16 }
    | CPMethodType { descriptorIndex :: Word16 }
    | CPInvokeDynamic { bootstrapMethodAttrIndex :: Word16, nameAndTypeIndex :: Word16 }
      deriving Show

  {-
    Parse functions
  -}

  parseConstants :: Parser [CPConstant]
  parseConstants = ((`subtract` 1) <$> getWord16i) >>= flip replicateM parseConstant

  parseConstant :: Parser CPConstant
  parseConstant = do
    tag <- getWord8
    case tag of
      7 -> CPClass <$> getWord16
      9 -> CPFieldref <$> getWord16 <*> getWord16
      10 -> CPMethodref <$> getWord16 <*> getWord16
      11 -> CPInterfaceMethodref <$> getWord16 <*> getWord16
      8 -> CPString <$> getWord16
      3 -> CPInteger <$> getWord32
      4 -> CPFloat <$> getWord32
      5 -> CPLong <$> getWord32 <*> getWord32
      6 -> CPDouble <$> getWord32 <*> getWord32
      12 -> CPNameAndType <$> getWord16 <*> getWord16
      1 -> CPUtf8 <$> (getWord16i >>= flip replicateM getWord8)
      15 -> CPMethodHandle <$> getWord8 <*> getWord16
      16 -> CPMethodType <$> getWord16
      18 -> CPInvokeDynamic <$> getWord16 <*> getWord16
      _ -> error ("Bad Constant Tag: " ++ show tag)
