module MateVMRuntime.GarbageAlloc where

import MateVMRuntime.Types
import Foreign
import Foreign.C

mallocClassData :: Int -> IO (Ptr a)
mallocStaticData :: Int -> FieldTypeMap -> IO (Ptr a)
mallocObjectUnmanaged :: Int -> IO CPtrdiff
mallocStringUnmanaged :: Int -> IO (Ptr a)
mallocStringGC :: Int -> IO (Ptr a)
mallocObjectGC :: Int -> IO CPtrdiff
mallocObjectGC_stackstrace :: CPtrdiff -> CPtrdiff -> Int -> IO CPtrdiff


printGCStats :: IO ()
printMemoryUsage :: IO ()
getHeapMemory :: IO Int
