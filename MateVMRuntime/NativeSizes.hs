{-# LANGUAGE CPP #-}
module MateVMRuntime.NativeSizes where

import Data.Word

ptrSize, longSize :: Num a => a
ptrSize = 4
longSize = 8

type NativeWord = Word32
