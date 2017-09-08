module Parser.Class.Constants where
  import Parser.Class.Types

  -- Imports for helper methods
  import Parser.Class.Helpers (getWord8, getWord16, getWord16i, getWord32)
  import Control.Monad (replicateM, when)

  {-
    Types for Constant Pool
  -}

  {-
    Parse functions
  -}

  parseConstants :: Parser [CPConstant]
  parseConstants = ((`subtract` 1) <$> getWord16i) >>= flip replicateM parseConstant

  -- The JVM Specification states that both CPLong and CPDouble are 'double width'
  -- in that they take up two entries instead of one. Hence, when we read one, we
  -- need to ensure that we skip an additional slot. To do this, we store whether
  -- we need to skip over an iteration as our user-state.
  parseConstant :: Parser CPConstant
  parseConstant = do

    -- Check if we need to skip and reset state if we do.
    _status <- getStatus
    if _status == skip then setStatus normal >> return CPDummy else do

    -- Check if we are processing double-width constant and mark that
    -- we need to skip next time.
    tag <- getWord8
    when (tag == 5 || tag == 6) $ setStatus skip

    -- Parse the actual constants based on their tag.
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
