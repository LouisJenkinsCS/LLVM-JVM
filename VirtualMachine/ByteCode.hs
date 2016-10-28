module VirtualMachine.ByteCode where
  import Data.IORef
  import VirtualMachine.Types
  import VirtualMachine.Stack_Frame

  execute :: StackFrame -> Instructions -> IO ()
  execute frame instrRef = do
    mnemonic <- getNextBC instrRef
    case mnemonic of
      1 -> pushOp 0 frame
      _ -> return ()

  getNextBC :: Instructions -> IO ByteCode
  getNextBC instrRef = do
    instr <- readIORef instrRef
    modifyIORef instrRef tail
    return $ head instr
