module DataTypes.Class_File where
  import Data.Word
  import Data.ByteString.Char8 as CBS
  import qualified Data.ByteString as BS
  import DataTypes.Constant_Pool
  import DataTypes.Parse_Bytes
  import Control.Monad.State.Lazy
  import DataTypes.Attributes
  import DataTypes.Methods
  import DataTypes.Fields
  import VirtualMachine.Stack

  {-
    Data structure representation of a `.class` file.
  -}
  data ClassFile = ClassFile {
      cp_info :: [CP_Info],
      access_flags :: Word16,
      this_class :: Word16,
      super_class :: Word16,
      interfaces :: [Word16],
      fields :: [Field_Info],
      methods :: [Method_Info],
      classfile_attributes :: [Attribute_Info]
  } deriving (Show)

  parseClassFile :: ByteString -> ClassFile
  parseClassFile = evalState $ getNextInt >> getNextShort >> getNextShort -- Discard magic and version information
    >> parseConstants >>= \cp -> ClassFile cp <$> getNextShort <*> getNextShort <*> getNextShort
    <*> parseInterfaces <*> parseFields cp <*> parseMethods cp <*> parseAttributes cp
      where
        parseInterfaces = getNextShort >>= \n -> replicateM (fromIntegral n) getNextShort

  main :: IO ()
  main = do
    r0 <- BS.readFile "ClassFiles/test.class"
    Prelude.putStrLn $ "Starting Bytes: " ++ show (BS.length r0)
    let classFile = parseClassFile r0
    Prelude.putStrLn $ show classFile
    stack <- bootstrap
    str <- debugStack stack
    Prelude.putStrLn $ "Pre-Stack: " ++ str
    pushFrame stack 1 [4,132,132,132,59]
    Prelude.putStrLn $ "Setup Stack..."
    -- let codeAttr = getCodeAttribute (cp_info classFile) (classfile_attributes classFile)
    -- let code' = code codeAttr
    str' <- debugStack stack
    Prelude.putStrLn $ "Post-Stack: " ++ str'
    debugExec stack
    str'' <- debugStack stack
    Prelude.putStrLn $ "Final-Stack: " ++ str''
