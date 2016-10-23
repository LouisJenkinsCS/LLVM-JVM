module DataTypes.Parse_Bytes (Parser(), bsOfSize, getNextBytes, getNext, getNextInt, getNextByte, getNextShort, getNextHex) where
  import Data.Bits (shift, (.|.), Bits())
  import Data.ByteString.Char8
  import Data.ByteString
  import Data.ByteString.Base16
  import Prelude hiding (splitAt)
  import Data.Word
  import Control.Arrow
  import Control.Monad.State.Lazy

  type Parser = State ByteString

  getNext :: (Bits a, Num a) => Int -> Parser a
  getNext = fmap toBigEndian . bsOfSize

  getNextBytes :: Int -> Parser [Word8]
  getNextBytes = fmap Data.ByteString.unpack . bsOfSize

  bsOfSize :: Int -> Parser ByteString
  bsOfSize = state . Data.ByteString.splitAt

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

  -- getNext :: (Bits a, Num a) => Int -> ByteString -> (a, ByteString)
  -- getNext n bstr = let
  --   (b, bstr') = splitAt n bstr
  --   in (Data.ByteString.foldl' (\x y -> x `shift` 8 .|. fromIntegral y) 0 b, bstr')
