module DataTypes.Class_File where
  import Data.Word
  import Data.ByteString.Char8 as CBS
  import qualified Data.ByteString as BS
  import ClassFile.Parser
  import Control.Monad.State.Lazy
  import ClassFile.Types
  import VirtualMachine.Environment





  main :: IO ()
  main = do
    r0 <- BS.readFile "AdditionTest.class"
    Prelude.putStrLn $ "Starting Bytes: " ++ show (BS.length r0)
    let classFile = parseClassFile r0
    Prelude.putStrLn $ show classFile
    Prelude.putStrLn "Initializing Runtime Environment..."
    env <- VirtualMachine.Environment.init
    Prelude.putStrLn "Loading bootstrap class..."
    loadClass env classFile
    Prelude.putStrLn "Starting Virtual Machine..."
    VirtualMachine.Environment.start env
