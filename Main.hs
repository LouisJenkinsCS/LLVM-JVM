module Main where
  import GHC.IO.Encoding
  import qualified Data.ByteString as BS
  import ClassFile.Parser
  import VirtualMachine.Environment
  import Control.Monad
  import Data.IORef
  import System.Environment;
  import System.IO
  import Misc.Logger



  main :: IO ()
  main = do
    setLocaleEncoding utf8
    hSetBuffering stdout LineBuffering
    file <- getArgs >>= BS.readFile . head
    let classFile = parseClassFile file
    debugIO $ show classFile
    debugIO "Initializing Runtime Environment..."
    env <- VirtualMachine.Environment.init
    debugIO "Loading bootstrap class..."
    loadClass env classFile
    debugIO "Starting Virtual Machine..."
    VirtualMachine.Environment.start env
