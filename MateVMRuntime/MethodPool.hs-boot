{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module MateVMRuntime.MethodPool
  ( lookupMethodEntry
  , executeFuncPtr
  ) where

import MateVMRuntime.Types
import MateVMRuntime.NativeSizes
import Foreign.C.Types

lookupMethodEntry :: MethodInfo -> IO CPtrdiff
executeFuncPtr :: NativeWord -> IO ()
