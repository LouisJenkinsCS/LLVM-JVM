{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Minimal_JVM (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,0,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Louis\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Louis\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\Minimal-JVM-1.0.0.0-LP4g3l5fYZw2z6r6PMcWEw"
datadir    = "C:\\Users\\Louis\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.0.1\\Minimal-JVM-1.0.0.0"
libexecdir = "C:\\Users\\Louis\\AppData\\Roaming\\cabal\\Minimal-JVM-1.0.0.0-LP4g3l5fYZw2z6r6PMcWEw"
sysconfdir = "C:\\Users\\Louis\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Minimal_JVM_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Minimal_JVM_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Minimal_JVM_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Minimal_JVM_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Minimal_JVM_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
