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
    -- Constants
    | bc >= 1 && bc <= 15 = constOp frame bc
    -- BIPUSH BYTE
    | bc == 16 = getNextBC frame >>= pushOp frame . fromIntegral
    -- SIPUSH BYTE1 BYTE2
    | bc == 17 = replicateM 2 (getNextBC frame) >>= \(b1:b2:_) -> pushOp frame (fromIntegral b1 `shift` 8 .|. fromIntegral b2)
    -- LDC* (TODO)
    -- Loads
    | bc >= 21 && bc <= 53 = loadOp frame bc
    -- Stores
    | bc >= 54 && bc <= 86 = storeOp frame bc
    -- Math
    | bc >= 96 && bc <= 132 = mathOp frame bc
    -- Return
    | bc == 177 = return ()
    | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc

  execute :: StackFrame -> IO ()
  execute frame = do
    f <- readIORef frame
    let instr = byte_code . code_segment $ f
    case length instr of
      0 -> return ()
      _ -> do
        bc <- getNextBC frame
        execute' frame bc instr
        execute frame


  constOp :: StackFrame -> ByteCode -> IO ()
  constOp frame bc
    -- ACONST_NULL
    | bc == 1 = pushOp frame (VReference 0)
    -- ICONST_*
    | bc >= 2 && bc <= 8 = pushOp frame (VInt (fromIntegral $ bc - 3))
    -- LCONST_*
    | bc == 9 || bc == 10 = pushOp frame (VLong (fromIntegral $ bc - 9))
    -- FCONST_*
    | bc >= 11 && bc <= 13 = pushOp frame (VFloat (fromIntegral $ bc - 11))
    -- DCONST_*
    | bc >= 14 && bc <= 15 = pushOp frame (VDouble (fromIntegral $ bc - 14))
    --
    | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc

  loadOp :: StackFrame -> ByteCode -> IO ()
  loadOp frame bc
    -- Loads which have the index as the next bytecode instruction
    | bc >= 21 && bc <=25 = getNextBC frame >>= getLocal frame >>= pushOp frame
    -- Loads which have a constant index (I.E: ILOAD_0 to ILOAD_3)
    | bc >= 26 && bc <= 45 = getLocal frame ((bc - 26) `mod` 4) >>= pushOp frame
    | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc

  storeOp :: StackFrame -> ByteCode -> IO ()
  storeOp frame bc
    -- Stores which have the Index as the next bytecode instruction
    | bc >= 54 && bc <= 58 = popOp frame >>= \op -> getNextBC frame >>= \idx -> putLocal frame idx op
      >> when (bc == 55 || bc == 57) (putLocal frame (idx + 1) (VReference 0))
    -- Stores which have a constant index
    | bc >= 59 && bc <= 78 = popOp frame >>= putLocal frame ((bc - 59) `mod` 4)
      >> when ((bc >= 63 && bc <= 66) || (bc >= 71 && bc <= 74)) (putLocal frame (((bc - 59) `mod` 4) + 1) (VReference 0))
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
