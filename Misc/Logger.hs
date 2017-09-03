module Misc.Logger where
  import Debug.Trace

  -- Determines whether or not we print log messages
  debugMode :: Bool
  debugMode = False

  -- Logs if `debug` is True
  debug :: String -> ()
  debug str = if debugMode then trace ("DEBUG: " ++ str) () else ()

  -- Wrappers for when inside IO monad. This method is strict and evaluated immediately.
  debugM :: Monad m => String -> m ()
  debugM str = return $! debug str
