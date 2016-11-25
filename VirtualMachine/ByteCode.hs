module VirtualMachine.ByteCode where
  import Data.IORef
  import Data.Bits
  import Data.Word
  import Control.Monad
  import VirtualMachine.Types
  import ClassFile.Types
  import VirtualMachine.Debug
  import VirtualMachine.Stack_Frame
  import Data.Binary.IEEE754

  loadConstantPool :: Runtime_Environment -> Int -> IO Value
  loadConstantPool env idx = (!! idx) . constant_pool <$> readIORef (current_class env) >>= toValue
    where
      toValue :: CP_Info -> IO Value
      toValue info = case tag info of
        3 -> return . VInt . fromIntegral . bytes $ info
        4 -> return . VFloat . wordToFloat . bytes $ info
        5 -> return . VLong $ (fromIntegral . high_bytes $ info) `shift` 32  .|. (fromIntegral . low_bytes $ info)
        6 -> return . VDouble . wordToDouble $ (fromIntegral . high_bytes $ info) `shift` 32  .|. (fromIntegral . low_bytes $ info)
        8 -> readIORef (current_class env) >>= \c -> (return . VString . show . utf8_bytes) (constant_pool c !! (fromIntegral . string_index $ info))
        _ -> error $ "Bad Tag: " ++ show (tag info)

  {- Starting point of execution of ByteCode isntructions -}
  execute :: Runtime_Environment -> IO ()
  execute env = head <$> readIORef (stack env)  -- Take the head of the stack (current stack frame)
    >>= \frame -> when (debug_mode env) (debugFrame frame >>= putStrLn) -- Optional Debug
    >> readIORef frame >>= \f -> readIORef (program_counter . code_segment $ f) >>= \pc ->
    unless (fromIntegral pc >= length (byte_code . code_segment $ f)) -- While valid program_counter
    (getNextBC frame >>= execute' frame >> execute env) -- Execute instruction
      where
        -- The main dispatcher logic
        execute' :: StackFrame -> ByteCode -> IO ()
        execute' frame bc
          -- NOP
          | bc == 0 = return ()
          -- Constants
          | bc >= 1 && bc <= 15 = constOp frame bc
          -- Push raw byte(s)
          | bc == 16 || bc == 17 =
            -- 0x10 pushes a single byte, but 0x11 pushes a short
            (if bc == 16 then fromIntegral <$> getNextBC frame else getNextShort frame)
              >>= pushOp frame . fromIntegral
          -- Load constant pool constant
          | bc >= 18 && bc <= 20 =
            -- Only 0x12 uses only one byte, so we add a special case
            (if bc == 18 then fromIntegral <$> getNextBC frame else getNextShort frame)
              >>= loadConstantPool env . fromIntegral >>= pushOp frame
          -- Loads
          | bc >= 21 && bc <= 53 = loadOp frame bc
          -- Stores
          | bc >= 54 && bc <= 86 = storeOp frame bc
          -- Special Case: 'dup' is used commonly but ignored, so we have to stub it
          | bc == 89 = return ()
          -- Math
          | bc >= 96 && bc <= 132 = mathOp frame bc
          -- Conditionals
          | bc >= 148 && bc <= 166 = cmpOp frame bc
          -- Goto
          | bc == 167 = (program_counter . code_segment <$> readIORef frame) >>= readIORef
            >>= \pc -> getNextShort frame >>= \n -> readIORef frame >>= flip (writeIORef . program_counter . code_segment) (fromIntegral (fromIntegral pc + n - 1))
          -- Return
          | bc == 177 = return ()
          -- Runtime Stubs
          | bc >= 178 || bc <= 195 = runtimeStub env frame bc
          | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc

  runtimeStub :: Runtime_Environment -> StackFrame -> ByteCode -> IO ()
  runtimeStub env frame bc
    -- getstatic: 2 bytes wide
    | bc == 178 = void $ getNextShort frame
    -- invokevirtual: (append, println). NOTE: MUST HAVE ONLY ONE PARAMETER ELSE UNDEFINED
    | bc == 182 = getNextShort frame >>= \method_idx -> (readIORef . current_class) env
      >>= \c -> case methodName c method_idx of
        "append" -> ((\x y -> VString $ show y ++ show x) <$> popOp frame <*> popOp frame)
          >>= pushOp frame
        "println" -> popOp frame >>= print
        "toString" -> return ()
        _ -> error "Bad Method Call!"
    -- Used to call <init>, but we don't deal with that... yet
    | bc == 183 = void $ getNextShort frame
    -- Laziness: 'new' must refer to StringBuilder... otherwise it's undefined anyway
    | bc == 187 = getNextShort frame >> pushOp frame (VString "")
        where
          methodName :: Class -> Word16 -> String
          methodName clazz method_idx = let
            cpool = constant_pool clazz
            method_ref = cpool !! fromIntegral method_idx
            name_and_type = cpool !! fromIntegral (name_and_type_index method_ref)
            utf8_name = cpool !! fromIntegral (name_index name_and_type)
            in show . utf8_bytes $ utf8_name

  {-
    Loads transfer values from a slot in the Local Variable array to the Operand
    Stack. Note as well that we can safely ignore the second index for DWORD-sized
    variables.
  -}
  loadOp :: StackFrame -> ByteCode -> IO ()
  loadOp frame bc
    -- Loads which have the index as the next bytecode instruction
    | bc >= 21 && bc <=25 = getNextBC frame >>= getLocal frame >>= pushOp frame
    -- Loads which have a constant index (I.E: ILOAD_0 to ILOAD_3)
    | bc >= 26 && bc <= 45 = getLocal frame ((bc - 26) `mod` 4) >>= pushOp frame
    | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc

  {-
    Stores transfer the top value of the Operand Stack to an indice in the
    Local Variable array. The JVM specification specifies that for DWORD sized
    types (I.E: Long and Double) take up two slots in the Local Variable
    array (the lower index contains the higher WORD, higher index contains the
    lower WORD), so we store a dummy value (Null reference) in the higher index.
  -}
  storeOp :: StackFrame -> ByteCode -> IO ()
  storeOp frame bc
    -- Stores which have the Index as the next bytecode instruction
    | bc >= 54 && bc <= 58 = popOp frame >>= \op -> getNextBC frame >>= \idx -> putLocal frame idx op
      >> when (bc == 55 || bc == 57) (putLocal frame (idx + 1) (VReference 0))
    -- Stores which have a constant index
    | bc >= 59 && bc <= 78 = popOp frame >>= putLocal frame ((bc - 59) `mod` 4)
      >> when ((bc >= 63 && bc <= 66) || (bc >= 71 && bc <= 74))
      (putLocal frame (((bc - 59) `mod` 4) + 1) (VReference 0))
    | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc


  {- Stores constant values on the Operand Stack. -}
  constOp :: StackFrame -> ByteCode -> IO ()
  constOp frame bc
    -- Null Reference
    | bc == 1 = pushOp frame (VReference 0)
    -- Integer Constants
    | bc >= 2 && bc <= 8 = pushOp frame (VInt (fromIntegral $ bc - 3))
    -- Long Constants
    | bc == 9 || bc == 10 = pushOp frame (VLong (fromIntegral $ bc - 9))
    -- Float Constants
    | bc >= 11 && bc <= 13 = pushOp frame (VFloat (fromIntegral $ bc - 11))
    -- Double Constants
    | bc >= 14 && bc <= 15 = pushOp frame (VDouble (fromIntegral $ bc - 14))
    -- ERROR
    | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc

  {- Math operations which are abstracted by the Value type. -}
  mathOp :: StackFrame -> ByteCode -> IO ()
  mathOp frame bc
    | bc >= 96 && bc <= 99 = applyBinaryOp (+)
    | bc >= 100 && bc <= 103 = applyBinaryOp (-)
    | bc >= 104 && bc <= 107 = applyBinaryOp (*)
    | bc >= 108 && bc <= 111 = applyBinaryOp div
    | bc >= 112 && bc <= 115 = applyBinaryOp rem
    | bc >= 116 && bc <= 119 = applyUnaryOp negate
    -- Need to implement as lambda due to shiftL and shiftR requiring type Int
    | bc == 120 || bc == 121 = applyBinaryOp (\x y -> x `shiftL` fromIntegral y)
    | bc >= 122 && bc <= 125 = applyBinaryOp (\x y -> x `shiftR` fromIntegral y)
    | bc == 126 || bc == 127 = applyBinaryOp (.&.)
    | bc == 128 || bc == 129 = applyBinaryOp (.|.)
    | bc == 130 || bc == 131 = applyBinaryOp xor
    | bc == 132 = increment
    | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc
      where
        applyUnaryOp :: (Operand -> Operand) -> IO ()
        applyUnaryOp f = popOp frame >>= pushOp frame . f
        applyBinaryOp :: (Operand -> Operand -> Operand) -> IO ()
        applyBinaryOp f = replicateM 2 (popOp frame) >>= \(x:y:_) -> pushOp frame (f y x)
        increment :: IO ()
        increment = getNextBC frame >>= \idx -> getLocal frame idx >>= \l -> getNextBC frame >>= \n -> putLocal frame idx (l + fromIntegral n)

  cmpOp :: StackFrame -> ByteCode -> IO ()
  cmpOp frame bc
    | bc >= 148 && bc <= 152 = pushCmp
    | bc >= 153 && bc <= 166 = (program_counter . code_segment <$> readIORef frame) >>= readIORef
      >>= \pc -> getNextShort frame >>= \jmp -> when (bc >= 159) pushCmp >> cmpJmp (fromIntegral pc + jmp - 1)
    | otherwise = error $ "Bad ByteCode Instruction: " ++ show bc
    where
      pushCmp :: IO ()
      pushCmp = (flip compare <$> popOp frame <*> popOp frame) >>= pushOrd
      pushOrd :: Ordering -> IO ()
      pushOrd ord = pushOp frame (VInt (ordToInt ord))
      cmpJmp :: Word16 -> IO ()
      cmpJmp
        | bc == 153 || bc == 159 || bc == 165 = flip condJmp [EQ]
        | bc == 154 || bc == 160 || bc == 166 = flip condJmp [LT, GT]
        | bc == 155 || bc == 161 = flip condJmp [LT]
        | bc == 156 || bc == 162 = flip condJmp [GT, EQ]
        | bc == 157 || bc == 163 = flip condJmp [GT]
        | bc == 158 || bc == 164 = flip condJmp [LT, EQ]
      condJmp :: Word16 -> [Ordering] -> IO ()
      condJmp jmp ord = popOp frame >>= \op -> when (intToOrd op `elem` ord)
        (readIORef frame >>= flip (writeIORef . program_counter . code_segment) (fromIntegral jmp))
      ordToInt ord = case ord of
        GT -> 1
        EQ -> 0
        LT -> -1
      intToOrd int = case int of
        1 -> GT
        0 -> EQ
        -1 -> LT




  getNextBC :: StackFrame -> IO ByteCode
  getNextBC frame = readIORef frame >>= \f -> -- Read StackFrame from passed reference
    let segment = code_segment f in -- Retrieve current set of instructions
    readIORef (program_counter segment) >>= \n -> -- Read current ByteCode instruction
    modifyIORef (program_counter segment) (+1) >> -- Increment PC
    return (byte_code segment !! fromIntegral n) -- Return ByteCode instruction

  getNextShort :: StackFrame -> IO Word16
  getNextShort frame = replicateM 2 (getNextBC frame) >>= \(i1:i2:_) -> return $ fromIntegral i1 `shift` 8 .|. fromIntegral i2
