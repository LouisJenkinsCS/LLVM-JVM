module Main where
  import GHC.IO.Encoding
  import qualified Data.ByteString as BS
  import ClassFile.Parser
  import VirtualMachine.Environment
  import Control.Monad
  import Data.IORef
  import System.IO

  -- Status flags for input
  dummyFlag = "DUMMY"
  selectedJava = "Examples/Java/"
  selectedScala = "Examples/Scala/"
  selectedCustom = "CUSTOM"

  processInput :: Int -> IO String
  processInput n = case n of
    -- Toggle Debug
    0 -> return dummyFlag
    -- Java
    1 -> return selectedJava
    -- Scala
    2 -> return selectedScala
    -- Custom Input
    3 -> return selectedCustom
    -- Bad Input
    _ -> error $ "Bad Input: " ++ show n

  processFileSelection :: String -> Int -> IO String
  processFileSelection prefix n = case n of
    -- Arithmetic Test
    1 -> return $ prefix ++ "ArithmeticTest.class"
    -- Conditional Test
    2 -> return $ prefix ++ "ConditionalTest.class"
    -- Bad Input
    _ -> error $ "Bad Input: " ++ show n

  askInput :: IORef Bool -> IO String
  askInput debug = read <$> (putStrLn "0) Toggle Debug. 1) Java. 2) Scala. 3) Custom" >> getLine)
    >>= processInput >>= \opt -> if opt == dummyFlag then modifyIORef debug not
      >> askInput debug else if opt == selectedCustom then getLine
        else read <$> (putStrLn "1) Arithmetic Test. 2) Conditional Test." >> getLine)
          >>= processFileSelection opt

  main :: IO ()
  main = do
    debug <- newIORef False
    setLocaleEncoding utf8
    hSetBuffering stdout LineBuffering
    file <- askInput debug >>= BS.readFile
    let classFile = parseClassFile file
    flip when (print classFile) <$> readIORef debug
    flip when (putStrLn "Initializing Runtime Environment...") <$> readIORef debug
    env <- readIORef debug >>= VirtualMachine.Environment.init
    flip when (putStrLn "Loading bootstrap class...") <$> readIORef debug
    loadClass env classFile
    flip when (putStrLn "Starting Virtual Machine...") <$> readIORef debug
    VirtualMachine.Environment.start env
