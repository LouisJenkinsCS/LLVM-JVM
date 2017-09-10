module Parser.Class.Attributes where
  import Parser.Class.Types

  import Data.ByteString.Char8 (unpack)
  import Data.ByteString (pack)

  -- Imports for helper methods
  import Parser.Class.Helpers
  import Control.Monad (replicateM)

  parseAttributes :: Parser [Attribute]
  parseAttributes = getWord16i >>= flip replicateM parseAttribute

  parseAttribute :: Parser Attribute
  parseAttribute = do
    -- The next 16 bits is an index into the constant pool that holds a UTF8 constant.
    -- As well, each attribute can only be distinguished by its name, so we must
    -- retrieve it first to determine how to parse it next.
    name <- (unpack . pack . utf8Bytes) <$> (getWord16 >>= getConstant)
    -- The next 32-bits determine the overall length of this attribute (not counting
    -- what we have found so far). This length is useful for when we not support
    -- the attribute (in order to safely skip over it), however when we do know
    -- the exact attribute, we may safely ignore it
    len <- getWord32i
    -- Now, we parse the rest of it based on the attribute name.
    case name of
      -- Constants are indexes into the constant pool. As these constants may vary
      -- in type (Int vs Double vs String vs etc.) we only know the actual type
      -- later.
      "ConstantValue" -> ConstantValue <$> getWord16
      -- Denotes the actual executable code. The code segment also holds both
      -- the maximum amount of operands and local variables alive at one time.
      "Code" -> CodeAttribute <$> getWord16 <*> getWord16 <*> parseCode
        <*> parseExceptionTable <*> parseAttributes
        where
          parseCode = getWord32i >>= flip replicateM getWord8
          parseExceptionTable = getWord16i >>= flip replicateM parseExceptionEntry
            where
              parseExceptionEntry = ExceptionEntry <$> getWord16 <*> getWord16
                <*> getWord16 <*> getWord16
      -- Not supported yet...
      _ -> UnknownAttribute name <$> replicateM len getWord8
