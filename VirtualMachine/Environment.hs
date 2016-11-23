module VirtualMachine.Environment where
  import Data.IORef
  import qualified Data.Map as Map
  import VirtualMachine.Types
  import ClassFile.Types
  import VirtualMachine.Class
  import VirtualMachine.Stack
  import VirtualMachine.ByteCode
  import Data.Maybe

  init :: IO Runtime_Environment
  init = Environment <$> newIORef undefined <*> newIORef Map.empty <*> newIORef []

  loadClass :: Runtime_Environment -> ClassFile -> IO ()
  loadClass env cf = toClass cf >>= \c -> modifyIORef' (class_map env) (Map.insert classString c)
    >> writeIORef (current_class env) c
      where
        classString :: String
        classString = let
          cp = cp_info cf
          class_info = cp !! fromIntegral (this_class cf)
          name = cp !! fromIntegral (name_index class_info)
          in show $ utf8_bytes name

  start :: Runtime_Environment -> IO ()
  start env = putStrLn "Starting..." >> (fromJust . Map.lookup "main") <$> ((snd . head . Map.toList)
    <$> readIORef (class_map env) >>= readIORef . method_map)
    >>= pushFrame env >> putStrLn "Created stack for main..." >> execute env
