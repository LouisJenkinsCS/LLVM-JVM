module Main where
  import GHC.IO.Encoding
  import qualified Data.ByteString as BS
  import ClassFile.Parser
  import VirtualMachine.Environment

  main :: IO ()
  main = do
    let debug = False
    setLocaleEncoding utf8
    r0 <- BS.readFile "Examples/HelloWorld$.class"
    putStrLn $ "Starting Bytes: " ++ show (BS.length r0)
    let classFile = parseClassFile r0
    print classFile
    putStrLn "Initializing Runtime Environment..."
    env <- VirtualMachine.Environment.init debug
    putStrLn "Loading bootstrap class..."
    loadClass env classFile
    putStrLn "Starting Virtual Machine..."
    VirtualMachine.Environment.start env
