module Misc.Logger where
  import Debug.Trace
  import Control.Monad

  -- Determines whether or not we print log messages
  debugMode :: Bool
  debugMode = True

  -- Logs if `debug` is True
  debug :: String -> ()
  debug str = when debugMode (trace ("DEBUG: " ++ str)) ()

  -- Wrappers for when inside IO monad
  debugIO :: String -> IO ()
  debugIO = return . debug
