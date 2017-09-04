module Runtime.Environment where
  import Data.IORef
  import qualified Data.Map as Map
  import Runtime.Types
  import Parser.Types
  import Runtime.Class
  import Runtime.Stack
  import Runtime.ByteCode
  import Data.Maybe
  import Misc.Logger

  {- | Starting point for the runtime, invoked to instantiate the runtime environment -}
  init :: IO Runtime_Environment
  init = Environment <$> newIORef undefined -- 'current_class' is not available yet
    <*> newIORef Map.empty -- 'class_map' is originally empty
    <*> newIORef []  -- The 'stack' is initially empty

  {- | Minimal bootstrap class loader -}
  loadClass :: Runtime_Environment -> ClassFile -> IO ()
  loadClass env cf =
    toClass cf >>= \c -> modifyIORef' (class_map env) (Map.insert classString c)
      >> writeIORef (current_class env) c
      where
        classString :: String
        classString = let
          cp = cp_info cf
          class_info = cp !! fromIntegral (this_class cf)
          name = cp !! fromIntegral (name_index class_info)
          in show $ utf8_bytes name

  {- | Primer for the runtime, which sets up and executes the 'main' method -}
  start :: Runtime_Environment -> IO ()
  start env = (fromJust . Map.lookup "main") <$> -- Look for "main" function (result is a pair)
    ((snd . head . Map.toList) <$> -- Take the first (only) result and the second of pair (class)
    readIORef (class_map env) >>= readIORef . method_map) -- Obtain method_map of result class
    >>= pushFrame env >> execute env -- Create frame for main and begin execution
