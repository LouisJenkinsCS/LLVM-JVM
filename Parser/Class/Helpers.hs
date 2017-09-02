module Parser.Class.Helper where
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
  import Text.Parsec.ByteString
  import Text.Parsec ((<?>), count, anyChar)

  {-
    Custom Parsec Tokenizers. As Parsec does not directly support parsing operations
    directly on ByteStrings, we convert bytes to-and-from characters.
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

  -- Convert the bytes in big endian order. Note that this effectively reverses
  -- the entire ByteString, and as such only the portion of the ByteString to
  -- be parsed should be passed...
  toBigEndian :: (Bits a, Num a) => ByteString -> a
  toBigEndian = LazyByteString.foldl' (\x y -> x `shiftL` 8 .|. fromIntegral y) 0

  -- Helper method to obtain the next 'n' bytes in big endian order.
  readBytes :: (Integral a, Num b, Bits b) => a -> Parser b
  readBytes n = do
    val <- count (fromIntegral n) anyChar
    let valAsBytes = c2w `map` val in
      return $ toBigEndian . LazyByteString.pack $ valAsBytes


  {-
    Below we define helpful utility functions which return words of specific sizes.
  -}

  toWord8 :: Parser Word8
  toWord8 = readBytes (1 :: Int)

  toWord16 :: Parser Word16
  toWord16 = readBytes (2 :: Int)

  toWord32 :: Parser Word32
  toWord32 = readBytes (4 :: Int)

  toWord64 :: Parser Word64
  toWord64 = readBytes (8 :: Int)
