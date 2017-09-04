module Runtime.Core.GC where
  import Foreign.ForeignPtr

  {-
    A simple garbage collector that uses Haskell's 'ForeignPtr' API that allow
    automated tracking of memory.
  -}

  -- Allocates memory for the user
  alloc :: (Integral a) => a -> IO (ForeignPtr b)
  alloc = mallocForeignPtrBytes . fromIntegral
