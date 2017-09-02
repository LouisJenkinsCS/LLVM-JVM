module Parser.Class.Constants where
  import Text.Parsec.ByteString
  import Data.Word

  -- The Constant Pool consists of a 16-bit count (denoting the amount of constants
  -- this class file holds) followed by `cp_info` structures, the constants themselves.
  -- Note: All indexes are 0-based.
  parseConstants :: Parser [Word8]
  parseConstants = do
    
