module VirtualMachine.Class where
  import ClassFile.Types
  import VirtualMachine.Types
  import GHC.IORef
  import qualified Data.Map as Map

  toClass :: ClassFile -> IO Class
  toClass cf = Class <$> return (cp_info cf) <*> newIORef Map.empty <*> newIORef Map.empty
