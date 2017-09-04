module Main where
  import GHC.IO.Encoding
  import qualified Data.ByteString as LazyByteString
  import Runtime.Environment
  import System.Environment
  import Parser.Parser
  import System.IO
  import Misc.Logger


  main :: IO ()
  main = do
    setLocaleEncoding utf8
    hSetBuffering stdout LineBuffering
    file <- getArgs >>= LazyByteString.readFile . head
    let classFile = parseClassFile file
    debugM $ show classFile
    debugM "Initializing Runtime Environment..."
    env <- Runtime.Environment.init
    debugM "Loading bootstrap class..."
    loadClass env classFile
    debugM "Starting Virtual Machine..."
    Runtime.Environment.start env
