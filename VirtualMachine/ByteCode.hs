module VirtualMachine.ByteCode where
  import Data.IORef
  import Data.Bits
  import VirtualMachine.Types
  import VirtualMachine.Stack_Frame

  execute' :: StackFrame -> ByteCode -> Instructions -> IO ()
  execute' frame bc instrRef
    -- NOP
    | bc == 0 = return ()
    -- *CONST*
    | bc >= 1 && bc <= 15 = constOp frame bc
    -- BIPUSH BYTE
    | bc == 16 = do
      byte <- getNextBC instrRef
      pushOp (fromIntegral byte) frame
    -- SIPUSH BYTE1 BYTE2
    | bc == 17 = do
      byte1 <- getNextBC instrRef
      byte2 <- getNextBC instrRef
      pushOp (fromIntegral byte1 `shift` 8 .|. fromIntegral byte2) frame
    -- LDC* (TODO)
    -- *LOAD*
    | bc >= 21 && bc <= 53 = loadOp frame bc instrRef
    -- *STORE*
    | bc >= 54 && bc <= 86 = storeOp frame bc instrRef
    | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc

  execute :: StackFrame -> Instructions -> IO ()
  execute frame instrRef = do
    instr <- readIORef instrRef
    case length instr of
      0 -> return ()
      _ -> do
        bc <- getNextBC instrRef
        execute' frame bc instrRef
        execute frame instrRef


  constOp :: StackFrame -> ByteCode -> IO ()
  constOp frame bc
    -- ACONST_NULL
    | bc == 1 = pushOp 0 frame
    -- ICONST_*
    | bc >= 2 && bc <= 8 = pushOp (fromIntegral $ bc - 3) frame
    -- LCONST_*
    | bc == 9 || bc == 10 = pushOp (fromIntegral $ bc - 9) frame
    -- FCONST_*
    | bc >= 11 && bc <= 13 = pushOp (fromIntegral $ bc - 11) frame
    -- DCONST_*
    | bc >= 14 && bc <= 15 = pushOp (fromIntegral $ bc - 14) frame
    --
    | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc

  loadOp :: StackFrame -> ByteCode -> Instructions -> IO ()
  loadOp frame bc instrRef
    -- ILOAD || FLOAD || ALOAD
    | bc == 21 || bc == 23 || bc == 25 = do
      idx <- getNextBC instrRef
      local <- getLocalWORD (fromIntegral idx) frame
      pushOp (fromIntegral local) frame
    -- LLOAD || DLOAD
    | bc == 22 || bc == 24 = do
      idx <- getNextBC instrRef
      local <- getLocalDWORD (fromIntegral idx) frame
      pushOp (fromIntegral local) frame
    -- ILOAD_*
    | bc >= 26 && bc <= 29 = do
      local <- getLocalWORD (fromIntegral $ bc - 26) frame
      pushOp (fromIntegral local) frame
    -- LLOAD_*
    | bc >= 30 && bc <= 33 = do
      let idx = fromIntegral $ bc - 30
      local <- getLocalDWORD idx frame
      pushOp (fromIntegral local) frame
    -- FLOAD_*
    | bc >= 34 && bc <= 37 = do
      local <- getLocalWORD (fromIntegral $ bc - 34) frame
      pushOp (fromIntegral local) frame
    -- DLOAD_*
    | bc >= 38 && bc <= 41 = do
      let idx = fromIntegral $ bc - 38
      local <- getLocalDWORD idx frame
      pushOp (fromIntegral local) frame
    -- ALOAD_*
    | bc >= 42 && bc <= 45 = do
      local <- getLocalWORD (fromIntegral $ bc - 42) frame
      pushOp (fromIntegral local) frame
    -- *ALOAD (TODO)
    | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc

  storeOp :: StackFrame -> ByteCode -> Instructions -> IO ()
  storeOp frame bc instrRef
    -- ISTORE || FSTORE || ASTORE
    | bc == 54 || bc == 56 || bc == 58 = do
      idx <- getNextBC instrRef
      operand <- popOp frame
      putLocal (fromIntegral idx) (fromIntegral operand) frame
    -- LSTORE || DSTORE
    | bc == 55 || bc == 57 = do
      idx <- getNextBC instrRef
      operand <- popOp frame
      let high = operand `shiftR` 32
      let low = operand .&. 0xFFFFFFFF
      putLocal (fromIntegral idx) (fromIntegral high) frame
      putLocal (fromIntegral $ idx + 1) (fromIntegral low) frame
    -- ISTORE_*
    | bc >= 59 && bc <= 62 = do
      operand <- popOp frame
      putLocal (fromIntegral $ bc - 59) (fromIntegral operand) frame
    -- LSTORE_*
    | bc >= 63 && bc <= 66 = do
      let idx = fromIntegral $ bc - 63
      operand <- popOp frame
      let high = operand `shiftR` 32
      let low = operand .&. 0xFFFFFFFF
      putLocal (fromIntegral idx) (fromIntegral high) frame
      putLocal (fromIntegral $ idx + 1) (fromIntegral low) frame
    -- FSTORE_*
    | bc >= 67 && bc <= 70 = do
      operand <- popOp frame
      putLocal (fromIntegral $ bc - 67) (fromIntegral operand) frame
    -- DSTORE_*
    | bc >= 71 && bc <= 74 = do
      let idx = fromIntegral $ bc - 71
      operand <- popOp frame
      let high = operand `shiftR` 32
      let low = operand .&. 0xFFFFFFFF
      putLocal (fromIntegral idx) (fromIntegral high) frame
      putLocal (fromIntegral $ idx + 1) (fromIntegral low) frame
    -- ASTORE_*
    | bc >= 75 && bc <= 78 = do
      operand <- popOp frame
      putLocal (fromIntegral $ bc - 75) (fromIntegral operand) frame
    -- *ASTORE (TODO)
    | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc

  getNextBC :: Instructions -> IO ByteCode
  getNextBC instrRef = do
    instr <- readIORef instrRef
    modifyIORef instrRef tail
    return $ head instr
