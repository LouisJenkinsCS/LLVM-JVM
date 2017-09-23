module MateVMRuntime.ClassHierarchy
  ( isInstanceOf
  , addClassEntry
  , addInterfaceEntry
  ) where

import qualified Data.ByteString.Lazy as B
import MateVMRuntime.NativeSizes

isInstanceOf :: NativeWord -> B.ByteString -> IO Bool
addClassEntry :: NativeWord -> NativeWord -> [B.ByteString] -> IO ()
addInterfaceEntry :: B.ByteString -> [B.ByteString] -> IO ()
