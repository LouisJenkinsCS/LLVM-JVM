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
