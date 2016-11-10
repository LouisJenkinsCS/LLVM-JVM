module DataTypes.Class_File where
  import Data.Word
  import Data.ByteString.Char8 as CBS
  import qualified Data.ByteString as BS
  import ClassFile.Parser
  import Control.Monad.State.Lazy
  import ClassFile.Types
  import VirtualMachine.Stack

  getMain :: [CP_Info] -> [Method_Info] -> Method_Info
  getMain cp (x:xs)
    | show (utf8_bytes (cp !! fromIntegral (method_name_index x))) == "main" = x
    | otherwise = getMain cp xs
  getMain _ [] = error "Main not found"

  getCodeAttribute :: [CP_Info] -> [Attribute_Info] -> Attribute_Info
  getCodeAttribute = findCodeAttribute
    where
      findCodeAttribute :: [CP_Info] -> [Attribute_Info] -> Attribute_Info
      findCodeAttribute cp (x:xs) = let
        nindex = attribute_name_index x
        name = show $ utf8_bytes $ cp !! fromEnum nindex
        in
          if name == "Code" then x else findCodeAttribute cp xs
      findCodeAttribute _ [] = undefined



  main :: IO ()
  main = do
    r0 <- BS.readFile "AdditionTest.class"
    Prelude.putStrLn $ "Starting Bytes: " ++ show (BS.length r0)
    let classFile = parseClassFile r0
    Prelude.putStrLn $ show classFile
    stack <- bootstrap
    str <- debugStack stack
    Prelude.putStrLn $ "Pre-Stack: " ++ str
    Prelude.putStrLn $ "Setup Stack..."
    let m = getMain (cp_info classFile) (methods classFile)
    let codeAttr = getCodeAttribute (cp_info classFile) (method_attributes m)
    pushFrame stack (max_locals codeAttr) (code codeAttr)
    str' <- debugStack stack
    Prelude.putStrLn $ "Post-Stack: " ++ str'
    debugExec stack
    str'' <- debugStack stack
    Prelude.putStrLn $ "Final-Stack: " ++ str''
