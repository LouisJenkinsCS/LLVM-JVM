module VirtualMachine.Environment where
  import Data.IORef
  import GHC.IOArray
  import Data.Word
  import qualified Data.Map as Map
  import VirtualMachine.Types
  import ClassFile.Types
  import VirtualMachine.Class

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
