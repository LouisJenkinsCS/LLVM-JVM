module Parser.Class.Helpers where
  -- Imports needed to operate on raw bytes
  import Data.Bits ((.|.), shiftL, Bits)
  import Data.Word (Word8, Word16, Word32, Word64)
  import Data.ByteString.Lazy (ByteString)
  import Data.ByteString.Internal (c2w)
  import qualified Data.ByteString.Lazy.Char8 as Char8 (pack)
  import qualified Data.ByteString.Lazy as LazyByteString

  -- Imports needed for parsing (Parsec)
  import Text.Parsec.Prim (tokens, tokenPrim)
  import Text.Parsec.Pos (updatePosChar, updatePosString)
  import Text.Parsec (Parsec, (<?>), count, anyChar)

  {-
    Types
  -}

  type Parser = Parsec ByteString Bool

  {-
    Custom Parsec Tokenizers.
  -}

  noneOf :: String -> Parser Word8
  noneOf cs = satisfy (\b -> b `notElem` [c2w c | c <- cs])

  char :: Char -> Parser Word8
  char c = byte (c2w c)

  byte :: Word8 -> Parser Word8
  byte c = satisfy (==c)  <?> show [c]

  satisfy :: (Word8 -> Bool) -> Parser Word8
  satisfy f = tokenPrim toString getNextPos matchToken
    where
      toString tok = show [tok]
      getNextPos pos tok _ = updatePosChar pos tok
      matchToken tok = if f (c2w tok) then Just (c2w tok) else Nothing

  string :: String -> Parser ByteString
  string s = Char8.pack <$> tokens show updatePosString s

  {-
    Core Parse Functions.
  -}

  toBigEndian :: (Bits a, Num a) => ByteString -> a
  toBigEndian = LazyByteString.foldl' (\x y -> x `shiftL` 8 .|. fromIntegral y) 0

  readBytes :: (Integral a, Num b, Bits b) => a -> Parser b
  readBytes n = do
    val <- count (fromIntegral n) anyChar
    let valAsBytes = c2w `map` val in
      return $ toBigEndian . LazyByteString.pack $ valAsBytes


  {-
    Convenience Parse Functions.
  -}

  getWord8 :: Parser Word8
  getWord8 = readBytes (1 :: Int)

  getWord8i :: (Integral a) => Parser a
  getWord8i = fromIntegral <$> getWord8

  getWord16 :: Parser Word16
  getWord16 = readBytes (2 :: Int)

  getWord16i :: (Integral a) => Parser a
  getWord16i = fromIntegral <$> getWord16

  getWord32 :: Parser Word32
  getWord32 = readBytes (4 :: Int)

  getWord32i :: (Integral a) => Parser a
  getWord32i = fromIntegral <$> getWord32

  getWord64 :: Parser Word64
  getWord64 = readBytes (8 :: Int)

  getWord64i :: (Integral a) => Parser a
  getWord64i = fromIntegral <$> getWord64
