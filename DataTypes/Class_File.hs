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

  {-
    Data structure representation of a `.class` file.
  -}
  data ClassFile = ClassFile {
      magic :: Word32,
      minor_version :: Word16,
      major_version :: Word16,
      constant_pool_count :: Word16,
      cp_info :: [CP_Info],
      access_flags :: Word16,
      this_class :: Word16,
      super_class :: Word16,
      interfaces_count :: Word16,
      interfaces :: [Word16],
      fields_count :: Word16,
      fields :: [Field_Info],
      methods_count :: Word16,
      methods :: [Method_Info],
      attributes_count :: Word16,
      attribute :: [Attribute_Info]
  } deriving (Show)

  parseClassFile :: ByteString -> ClassFile
  parseClassFile = evalState $ do
    m <- getNextInt
    minVer <- getNextShort
    maxVer <- getNextShort
    cp_count <- getNextShort
    cp <- replicateM (fromEnum cp_count - 1) parseConstant
    aflags <- getNextShort
    tclass <- getNextShort
    sclass <- getNextShort
    icount <- getNextShort
    ifaces <- replicateM (fromEnum icount) getNextShort
    fcount <- getNextShort
    flds <- replicateM (fromEnum fcount) (parseField $ Dummy_Info : cp)
    mcount <- getNextShort
    mthds <- replicateM (fromEnum mcount) (parseMethod $ Dummy_Info : cp)
    acount <- getNextShort
    attrs <- replicateM (fromEnum acount) (parseAttribute $ Dummy_Info : cp)
    return $ ClassFile m minVer maxVer cp_count (Dummy_Info : cp) aflags tclass sclass icount ifaces fcount flds mcount mthds acount attrs

  main :: IO ()
  main = do
    r0 <- BS.readFile "ClassFiles/test.class"
    Prelude.putStrLn $ "Starting Bytes: " ++ show (BS.length r0)
    let classFile = parseClassFile r0
    Prelude.putStrLn $ show classFile
