module VirtualMachine.Environment where
  import Data.IORef
  import GHC.IOArray
  import Data.Word
  import qualified Data.Map as Map
  import VirtualMachine.Types
  import ClassFile.Types
  import VirtualMachine.Class
  import Data.Maybe

  init :: IO Runtime_Environment
  init = Environment <$> newIORef Map.empty <*> newIORef []

  loadClass :: Runtime_Environment -> ClassFile -> IO ()
  loadClass env cf = toClass cf >>= \c -> modifyIORef' (class_map env) (Map.insert classString c)
      where
        classString :: String
        classString = let
          cp = cp_info cf
          class_info = cp !! fromIntegral (this_class cf)
          name = cp !! fromIntegral (name_index class_info)
          in show $ utf8_bytes name

  start :: Runtime_Environment -> IO ()
  start env = readIORef (class_map env) >>= Map.filter (\c -> readIORef (method_map c) >>= isJust . Map.lookup "main")

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
