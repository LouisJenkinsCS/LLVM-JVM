module VirtualMachine.ByteCode where
  import Data.IORef
  import Data.Bits
  import Control.Monad
  import VirtualMachine.Types
  import VirtualMachine.Stack_Frame
  import Data.Binary.IEEE754

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
    -- Math Operations
    | bc >= 96 && bc <= 132 = mathOp frame bc
    -- RETURN
    | bc == 177 = return ()
    | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc

  execute :: StackFrame -> IO ()
  execute frame = do
    f <- readIORef frame
    let instrRef = instructions f
    instr <- readIORef instrRef
    case length instr of
      0 -> return ()
      _ -> do
        bc <- getNextBC instrRef
        execute' frame bc instrRef
        execute frame


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

  loadOp :: StackFrame -> Code_Segment -> IO ()
  loadOp frame
    -- ILOAD || FLOAD || ALOAD
    | bc == 21 || bc == 23 || bc == 25 = do
      idx <- getNextBC frame
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

  storeOp :: StackFrame -> ByteCode -> Code_Segment -> IO ()
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

  mathOp :: StackFrame -> ByteCode -> IO ()
  mathOp frame bc
    | bc >= 96 && bc >= 99 = applyBinaryOp (+)
    | bc >= 100 && bc <= 103 = applyBinaryOp (-)
    | bc >= 104 && bc <= 107 = applyBinaryOp (*)
    | bc >= 108 && bc <= 111 = applyBinaryOp div
    | bc >= 112 && bc <= 115 = applyBinaryOp rem
    | bc >= 116 && bc <= 119 = applyUnaryOp negate
    -- Need to implement as lambda due to shiftL and shiftR requiring type Int
    | bc == 120 || bc == 121 = applyBinaryOp (\x y -> x `shiftL` fromIntegral y)
    | bc >= 122 || bc <= 125 = applyBinaryOp (\x y -> x `shiftR` fromIntegral y)
    | bc == 126 || bc == 127 = applyBinaryOp (.&.)
    | bc == 128 || bc == 129 = applyBinaryOp (.|.)
    | bc == 130 || bc == 131 = applyBinaryOp xor
    | bc == 132 = increment
      where
        applyUnaryOp :: (Operand -> Operand) -> IO ()
        applyUnaryOp f = popOp frame >>= pushOp frame . f
        applyBinaryOp :: (Operand -> Operand -> Operand) -> IO ()
        applyBinaryOp f = replicateM 2 (popOp frame) >>= \(x:y:_) -> pushOp frame (f x y)
        increment :: IO ()
        increment = getNextBC frame >>= getLocal frame >>= \l -> getNextBC frame >>= \n -> pushOp frame (l + fromIntegral n)

  getNextBC :: StackFrame -> IO ByteCode
  getNextBC frame = readIORef frame >>= \f -> -- Read StackFrame from passed reference
    let segment = code_segment f in -- Retrieve current set of instructions
    readIORef (program_counter segment) >>= \n -> -- Read current ByteCode instruction
    modifyIORef (program_counter segment) (+1) >> -- Increment PC
    return (byte_code segment !! fromIntegral n) -- Return ByteCode instruction
