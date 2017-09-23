module Main where
  import GHC.IO.Encoding

  import JVM.ClassFile
  import Java.JAR
  -- import Runtime.Environment
  import System.Environment
  import Data.Char
  import Data.List
  import Data.List.Split
  import qualified Data.ByteString.Lazy as B
  import qualified Data.ByteString.Lazy.Char8 as C8
  import Control.Monad

  import MateVMRuntime.ClassPool
  import MateVMRuntime.MethodPool

  import MateVMRuntime.Types
  import MateVMRuntime.Debug
  import MateVMRuntime.RtsOptions

  import MateVMRuntime.Utilities

  import Mate.GC.Boehm

  import Misc.Logger

  main ::  IO ()
  main = do
    time "main" $ do
      args <- getArgs
      parseArgs args False
    printCompileTime

  parseArgs :: [String] -> Bool -> IO ()
  parseArgs ("-jar":jarpath:_) stdcp = do
    unless stdcp $ addClassPath "./"
    addClassPathJAR jarpath
    res <- readMainClass jarpath
    case res of
      Nothing -> error "JAR: no MainClass entry found. Try to pass the jar file via -cp instead."
      Just mc -> do
        let bclspath = B.pack . map (fromIntegral . ord) $ mc
        cls <- getClassFile bclspath
        executeMain bclspath cls

  parseArgs ("-cp":cps) cpset = parseArgs ("-classpath":cps) cpset
  parseArgs ("-classpath":cps:xs) False = do
    mapM_ addStuff $ splitOn ":" cps
    parseArgs xs True
      where
        addStuff :: String -> IO ()
        addStuff x
          | ".jar" `isSuffixOf` x = addClassPathJAR x
          | otherwise = addClassPath $ x ++ "/"
  parseArgs ("-classpath":xs) _ = parseArgs ("-":xs) True -- usage
  parseArgs (('-':_):_) _ = error "Usage: mate [-cp|-classpath <cp1:cp2:..>] [<class-file> | -jar <jar-file>]"
  -- first argument which isn't prefixed by '-' should be a class file
  parseArgs (clspath:_) stdcp = do
    unless stdcp $ addClassPath "./"
    let bclspath = B.pack . map (fromIntegral . ord) $ clspath
    cls <- getClassFile bclspath
    executeMain bclspath cls
  parseArgs _ _ = parseArgs ["-"] False


  executeMain :: B.ByteString -> Class Direct -> IO ()
  executeMain bclspath cls = do
    --required on some platforms, initializes boehmgc. [todo bernhard: maybe this should be moved somewhere else - maybe at a global place where vm initialization takes place]
    unless usePreciseGC initGC

    case find ((==) (C8.pack "main") . methodName) (classMethods cls) of
      Just m -> do
        let mi = MethodInfo (C8.pack "main") bclspath $ methodSignature m
        entry <- lookupMethodEntry mi
        printfInfo "executing `main' now:\n"
        executeFuncPtr (fromIntegral entry)
        printfInfo "Well, goodbye Sir!\n"
      Nothing -> error "main not found"
