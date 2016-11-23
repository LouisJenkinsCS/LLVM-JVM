module ClassFile.Parser where
  import Data.Bits (shift, (.|.), Bits())
  import Data.ByteString.Char8
  import Data.ByteString
  import Data.ByteString.Base16
  import Prelude hiding (splitAt)
  import Data.Word
  import Control.Monad.State.Lazy
  import ClassFile.Types

  {-
    Parser implementation
  -}

  getNext :: (Bits a, Num a) => Int -> Parser a
  getNext = fmap toBigEndian . bsOfSize

  getNextBytes :: Int -> Parser [Word8]
  getNextBytes = fmap Data.ByteString.unpack . bsOfSize

  bsOfSize :: Int -> Parser ByteString
  bsOfSize = state . Data.ByteString.splitAt

  {-
    Converts the ByteString to big endian (highest-byte first) ordering. The compiler
    deduces the return type (Word8, Word16, Word32, etc.) based on the context of the caller.
    By using class constraints, we may bound the potential types of each generic type. In this
    case, the return type must be an instance of the class 'Bits' (I.E: Word8, Word16, etc.) and
    be an instance of class 'Num' (I.E: Int), of which the fixed WORD-sized types fulfill.

    "foldl'" is a functor that reduces a list of objects, essentially "folding" them all, into one.
    An example could be a way to accumulate the sum of a list of all numbers:

    fold (+) [1..10] => 55

    "foldl'" is special in that it will use the identity element (in this case, 0) for the initial value.
    'ByteString' supplies its own fold, which will apply it to each byte.
  -}
  toBigEndian :: (Bits a, Num a) => ByteString -> a
  toBigEndian = Data.ByteString.foldl' (\x y -> x `shift` 8 .|. fromIntegral y) 0

  {-
    Helpers for parsing, which parses N bytes from a passed ByteString, returning
    the value, as well as the remainder (simulating updating of state) of the ByteString.
    All bytes read are in Big Endian.
  -}
  getNextInt :: Parser Word32
  getNextInt  = getNext 4

  getNextShort :: Parser Word16
  getNextShort = getNext 2

  getNextByte :: Parser Word8
  getNextByte  = getNext 1

  -- Reads the next 'n' bytes, returning a tuple of the read bytes and remaining.
  getNextHex :: Int ->  Parser ByteString
  -- 'first' operator applies the functor, 'f', to the first element in the
  -- tuple, (a, b), such that it becomes (f a, b)
  getNextHex =  fmap encode . bsOfSize



  {-
    ClassFile Parser
  -}

  parseClassFile :: ByteString -> ClassFile
  parseClassFile = evalState $ getNextInt >> getNextShort >> getNextShort -- Discard magic and version information
    >> parseConstants >>= \cp -> ClassFile cp <$> getNextShort <*> getNextShort <*> getNextShort
    <*> parseInterfaces <*> parseFields cp <*> parseMethods cp <*> parseAttributes cp


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

  parseMethods :: [CP_Info] -> Parser [Method_Info]
  parseMethods cp = getNextShort >>= \n -> replicateM (fromIntegral n) parseMethod
    where
      parseMethod = Method_Info <$> getNextShort <*> getNextShort <*> getNextShort <*> parseAttributes cp

  parseFields :: [CP_Info] -> Parser [Field_Info]
  parseFields cp = getNextShort >>= \n -> replicateM (fromIntegral n) parseField
    where
      parseField = Field_Info <$> getNextShort <*> getNextShort <*> getNextShort <*> parseAttributes cp

  parseInterfaces :: Parser [Word16]
  parseInterfaces = getNextShort >>= \n -> replicateM (fromIntegral n) getNextShort
