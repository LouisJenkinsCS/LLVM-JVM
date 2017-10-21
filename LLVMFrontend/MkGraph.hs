{-RIPPED FROM MATEVM-}
{-# LANGUAGE RankNTypes #-}

module LLVMFrontend.MkGraph
  ( ParseState'(..)
  , addExceptionBlocks
  , resolveReferences
  , resetPC
  , mkBlocks
  , mkMethod
  ) where

  import qualified Data.List as L
  import qualified Data.Set as S
  import qualified Data.Map as M
  import qualified Data.IntervalMap as IM
  import qualified Data.IntervalMap.Interval as IIM
  import qualified Data.ByteString.Lazy as B
  import Data.Int
  import Data.Word
  import Data.Maybe

  import Control.Applicative hiding ((<*>))
  import Control.Monad
  import Control.Monad.State
  import Control.Arrow

  import qualified JVM.Assembler as J
  import JVM.Assembler hiding (Instruction)
  import JVM.ClassFile
  import Harpy hiding (Label, fst)

  import MateVMRuntime.Debug
  import MateVMRuntime.Types
  import MateVMRuntime.NativeSizes

  import Control.Monad.Extra

  import qualified Data.Vector as Vec
  import Data.Vector(Vector)

  import qualified LLVM.AST.Instruction as LI
  import qualified LLVM.AST.Global as LG
  import qualified LLVM.AST.Constant as LC
  import qualified LLVM.AST.Operand as LO
  import qualified LLVM.AST.Name as LN
  import qualified LLVM.AST.Type as LT
  import LLVMFrontend.Helpers

  -- import Debug.Trace

  data ParseState' = ParseState'
    { labels :: M.Map Int32 LN.Name {- store offset -> label -}
    , nBlocks :: Int
    , blockEntries :: S.Set Int32 {- store block entries (data gained from first pass) -}

    , pcOffset :: Int32 {- programm counter -}
    , operandStack :: [LO.Operand] {- simulation stack -}
    , autotmpN :: Int {- Counter for autotmp registers -}
    , classf :: Class Direct {- reference to class of processed method -}
    , method :: Method Direct {- reference to processed method -}

    , instructions :: [J.Instruction] {- instructions to process -}
    -- Note: The ExceptionMap is a type-alias for the IntervalMap, which maps an
    -- interval (I.E: [1,10] representing all numbers between 1 and 10) to its
    -- exception handler. This allows us to search whether or not a particular program
    -- counter has an exception handler within amortized O(log n) time.
    , exceptionMap :: ExceptionMap Int32 {- map of try-blocks, with references to handler -}
    , handlerStarts :: S.Set Int32 {- set of handler starts -}
    }

  type ParseState a = State ParseState' a

  -- Appends an instruction to the specified basic block by creating a copy containing
  -- the requested instruction.
  appendInstruction :: LI.Named LI.Instruction -> ParseState ()
  appendInstruction instr = do
    block <- head <$> gets basicBlocks
    updateCurrentBlock $ appendInstruction' instr block

    where
      appendInstruction' i (LG.BasicBlock n is t) = LG.BasicBlock n (is ++ [i]) t


  -- Generate Basic Block name
  generateBasicBlockName :: ParseState LN.Name
  generateBasicBlockName = do
    n <- gets nBlocks
    modify' $ \s -> s { nBlocks = n + 1 }
    return . LN.mkName $ "BB" ++ show n

  -- Split exception handlers and try-catch blocks into their own basic blocks.
  addExceptionBlocks :: ParseState ()
  addExceptionBlocks = do
    -- Each exception handler has incoming edges from any pc ∈ [pcStart, pcEnd]...
    hstarts <- gets S.toList . handlerStarts
    forM_ hstarts addPC
    exKeys <- gets IM.keys . exceptionMap
    -- The beginning of the try-block, pcStart, contains an incoming edge from the
    -- basic block preceeding us.
    forM_ (map IIM.lowerBound exKeys) addPC
    -- The end of the try-block, pcEnd, contains an outgoing edge to our successor, pcEnd + 1
    forM_ (map IIM.upperBound exKeys) $ addPC . (+1)

  -- forward references wouldn't be a problem, but backwards are
  resolveReferences :: ParseState ()
  resolveReferences = do
      jvminsn <- instructions <$> get
      pc <- pcOffset <$> get

      -- if there are no more JVM instructions, we are done here...
      if null jvminsn
        then do
          addPC 0 -- add entry instruction
          addPC pc -- mark return instruction
        else do
          when (null jvminsn) $ error "resolveReferences: something is really wrong here"
          let ins = head jvminsn
          addJumpTarget ins pc -- Handles whether this is a jump instruction or not
          incrementPC ins -- Also handles incrementing for multi-bytecode instructions
          resolveReferences
    where
      -- If the instruction is a jump instruction, we handle creating basic blocks
      -- for them as needed. If the instruction is not a jump instruction, this
      -- effectively becomes a NOP.
      addJumpTarget :: J.Instruction -> Int32 -> ParseState ()
      addJumpTarget ins pc = case ins of
          (IF _ rel) -> addPCs pc rel ins
          (IF_ICMP _ rel) -> addPCs pc rel ins
          (IF_ACMP _ rel) -> addPCs pc rel ins
          (IFNULL rel) -> addPCs pc rel ins
          (IFNONNULL rel) -> addPCs pc rel ins
          GOTO rel -> addPCs pc rel ins
          JSR _ -> error "addJumpTarget: JSR?!"
          GOTO_W _ -> error "addJumpTarget: GOTO_W?!"
          JSR_W _ -> error "addJumpTarget: JSR_W?!"
          TABLESWITCH _ def _ _ offs -> addSwitch pc def offs
          LOOKUPSWITCH _ def _ switch' -> addSwitch pc def $ map snd switch'
          _ -> return ()
      -- Add basic blocks for the _end_ of the current instruction (accounting for
      -- the size of the instruction) to its requested jump target instruction.
      addPCs :: Int32 -> Word16 -> J.Instruction -> ParseState ()
      addPCs pc rel ins = do
        addPC (pc + insnLength ins) -- Some instructions use more than one bytecode
        addPC (pc + w16Toi32 rel) -- Jump target for program counter

      -- Add a basic block for the switch instruction and all case statements.
      addSwitch :: Int32 -> Word32 -> [Word32] -> ParseState ()
      addSwitch pc def offs = do
        let addrel = addPC . (+ pc) . fromIntegral
        mapM_ addrel offs
        addrel def

  -- Creates a basic block entry for this program counter.
  addPC :: Int32 -> ParseState ()
  addPC bcoff = modify (\s -> s { blockEntries = S.insert bcoff (blockEntries s) })


  mkMethod :: ParseState ()
  mkMethod g = do
    hs <- handlerStarts <$> get
    -- Create labels for each exception handler, as well as for the entry block.
    forM_ addLabel (S.toList hs ++ [0])
    return ()

  mkBlocks :: ParseState ()
  mkBlocks = do
    pc <- pcOffset <$> get
    entries <- blockEntries <$> get
    jvminsn <- instructions <$> get

    if null jvminsn
      then return []
      else if S.member pc entries
        then do
          mkBlock
          mkBlocks
          return ()
        else error $ "mkBlocks: something wrong here. pc: " ++ show pc ++
                     "\ninsn: " ++ show jvminsn

  mkBlock :: ParseState ()
  mkBlock = do
    handlermap <- exceptionMap <$> get
    pc <- pcOffset <$> get
    l <- addLabel pc
    -- push LT.ptr for Exceptionblock, which is passed via %eax
    isExceptionHandler <- S.member pc <$> handlerStarts <$> get
    let handlerStart = if isExceptionHandler then Just . fromIntegral $ pc else Nothing

    -- Compute the exception handlers that we are apart of. As there cna be nested
    -- exception handlers/try-catch blocks, we end up with a list of the ones we
    -- are contained in.
    let extable = map (second fromIntegral)
                  $ concatMap snd
                  $ handlermap `IM.containing` pc
    -- let f' = IRLabel l extable handlerStart
    -- fixup block boundaries
    -- be <- -- trace (printf "pc: %d\nhstart: %s\nextable: %s\n" pc (show handlerStart) (show extable)) $
    --       M.lookup l <$> blockInterfaces <$> get
    -- fixup <- case be of
    --   Nothing -> return []
    --   Just ts -> forM ts $ \x -> do
    --                nv <- newvar $ varType x
    --                apush nv
    --                return $ IROp Add nv x (nul (varType x))
    (ms', l') <- toMid
    -- mkFirst f' <*> mkMiddles (fixup ++ ms') <*> mkLast l'
    return ()

  -- Add a label for the provided program counter, if not already.
  addLabel :: Int32 -> ParseState LN.Name
  addLabel boff = do
    lmap <- labels <$> get
    if M.member boff lmap
      then return $ lmap M.! boff -- Already present
      else do
        -- Construct a new label. We also create an outgoing edge to the label,
        -- as otherwise it would violate the basic block invariant.
        label <- generateBasicBlockName
        modify (\s -> s { labels = M.insert boff label (labels s) })
        return label

  -- Increment program counter, also  handling multibyte instructions.
  incrementPC :: J.Instruction -> ParseState ()
  incrementPC ins = do
    modify (\s -> s { pcOffset = pcOffset s + insnLength ins})
    popInstruction

  -- Advance current instruction. Note that the instruction is not returned, meaning
  -- the current instruction must be queried independently.
  popInstruction :: ParseState ()
  popInstruction = do
    i <- instructions <$> get
    when (null i) $ error "popInstruction: something is really wrong here"
    modify (\s -> s { instructions = tail i })

  toMid :: ParseState ()
  toMid = do
      pc <- pcOffset <$> get
      insns <- instructions <$> get
      when (null insns) $ error "toMid: something is really wrong here :/"
      ins <- head <$> instructions <$> get
      entries <- blockEntries <$> get
      if S.member (pc + insnLength ins) entries
        then do
          -- Refers to an already-existing basic block...
          res <- toLast ins
          incrementPC ins
          return res
        else do
          -- Points to an instruction, map to IR...
          insIR <- tir ins
          incrementPC ins
          first (insIR ++) <$> toMid
    where
      -- Handle conditional and unconditional jumps here. We create the appropriate
      -- basic blocks for the instructions as well.
      toLast :: J.Instruction -> ParseState ()
      toLast ins = do
        pc <- pcOffset <$> get
        -- Handle if-else statement, where we have two outgoing edges from the
        -- conditionally-executed basic block, and to the end of the basic block.
        let ifstuff jcmp rel op1 op2 = do
              truejmp <- addLabel (pc + w16Toi32 rel)
              falsejmp <- addLabel (pc + insnLength ins)
              error "Conditionals not supported..."
              -- return ([], IRIfElse jcmp op1 op2 truejmp falsejmp)
        -- Handle switch statements, adding outgoing edges to each case statement.
        let switchins def switch' = do
              y <- apop
              switch <- forM switch' $ \(v, o) -> do
                offset <- addLabel $ pc + fromIntegral o
                error "Conditionals not supported..."
                -- return (Just (w32Toi32 v), offset)
              defcase <- addLabel $ pc + fromIntegral def
              error "Conditionals not supported..."
              -- return ([], IRSwitch y $ switch ++ [(Nothing, defcase)])
        (ret1, ret2) <- case ins of
          RETURN -> return ([], retvoid)
          ARETURN -> returnSomething LT.i32
          IRETURN -> returnSomething LT.i32
          LRETURN -> returnSomething LT.i64
          FRETURN -> returnSomething LT.float
          DRETURN -> returnSomething LT.double
          (IF jcmp rel) -> error "Conditionals not supported..."
          (IFNULL rel) -> error "Conditionals not supported..."
          (IFNONNULL rel) -> error "Conditionals not supported..."
          (IF_ICMP jcmp rel) -> error "Conditionals not supported..."
          (IF_ACMP jcmp rel) -> error "Conditionals not supported..."
          (GOTO rel) -> error "Conditionals not supported..."
          TABLESWITCH _ def low high offs -> switchins def $ zip [low..high] offs
          LOOKUPSWITCH _ def _ switch -> switchins def switch
          _ -> do -- fallthrough case
            -- next <- addLabel (pc + insnLength ins)
            insIR <- tir ins
            error $ "Processing instruction: " ++ show insIR
        fixups <- handleBlockEnd
        return (ret1 ++ fixups, ret2)
        where
          -- Return the top operand (after performing type-checking)
          returnSomething t = do
            r <- apop
            error $ "Processing Type: " ++ show t ++ ", " ++ show r
            -- unless (varType r == t) $ error "toLast return: type mismatch"
            -- return ([], IRReturn $ Just r)

  -- handleBlockEnd :: ParseState [MateIR Var O O]
  -- handleBlockEnd = do
  --   st <- get
  --   let len = L.genericLength $ stack st
  --   if len > 0
  --     then
  --       forM [600000 .. (600000 + len - 1)] $ \r -> do
  --         x <- apop
  --         let vreg = VReg (VR r (varType x))
  --         targets <- nextTargets <$> get
  --         forM_ targets $ \t -> do
  --           be <- fromMaybe [] <$> M.lookup t <$> blockInterfaces <$> get
  --           modify (\s -> s { blockInterfaces = M.insert t (vreg:be) (blockInterfaces s)})
  --         return (IROp Add vreg x (nul (varType x)))
  --     else return []

  -- Calculates length of instruction, also taking into account whether or not
  -- it is multibyte.
  insnLength :: Integral a => J.Instruction -> a
  insnLength x = case x of
    (TABLESWITCH padding _ _ _ xs) ->
      fromIntegral $ 1 {- opcode -}
                   + padding
                   + (3 * 4) {- def, low, high -}
                   + 4 * L.genericLength xs {- entries -}
    (LOOKUPSWITCH padding _ _ xs) ->
      fromIntegral $ 1 {- opcode -}
                   + padding
                   + (2 * 4) {- def, n -}
                   + 8 * L.genericLength xs {- pairs -}
    -- TODO: better idea anyone?
    AALOAD -> 1
    AASTORE -> 1
    ALOAD_ _ -> 1
    ANEWARRAY _ -> 3
    ARETURN -> 1
    ARRAYLENGTH -> 1
    DUP -> 1
    GOTO _ -> 3
    ICONST_0 -> 1
    ICONST_1 -> 1
    ICONST_2 -> 1
    ICONST_3 -> 1
    ICONST_4 -> 1
    ICONST_5 -> 1
    IF_ICMP _ _ -> 3
    IF _ _ -> 3
    ILOAD_ _ -> 1
    INVOKESTATIC _ -> 3
    INVOKESPECIAL _ -> 3
    INVOKEVIRTUAL _ -> 3
    ISTORE_ _ -> 1
    NEW _ -> 3
    LDC1 _ -> 2
    LDC2 _ -> 3
    POP -> 1
    PUTFIELD _ -> 3
    PUTSTATIC _ -> 3
    RETURN -> 1
    SIPUSH _ -> 3
    _ -> len -- trace (printf "insn: %s -> len: %d\n" (show x) (fromIntegral len :: Word32)) len
    where
      len = fromIntegral . B.length . encodeInstructions . (:[]) $ x

  resetPC :: [J.Instruction] -> ParseState ()
  resetPC jvmins =
    modify (\s -> s { pcOffset = 0, instructions = jvmins })

  -- Converts a JVM Classfile IMM (immediate constant) to the actual number.
  -- For example, imm2num (ICONST_ x) will yield the actual number associated with it.
  imm2num :: Num a => IMM -> a
  imm2num I0 = 0
  imm2num I1 = 1
  imm2num I2 = 2
  imm2num I3 = 3

  fieldType :: Class Direct -> Word16 -> LT.Type
  fieldType cls off = fieldType2VarType $ ntSignature nt
    where nt = case constsPool cls M.! off of
                  (CField _ nt') -> nt'
                  _ -> error "fieldType: fail :("

  methodType :: Bool -> Class Direct -> Word16 -> ([LT.Type], Maybe LT.Type)
  methodType isVirtual cls off = (map fieldType2VarType argst', rett)
    where
      argst' = if isVirtual then ObjectType "this" : argst else argst
      (MethodSignature argst returnt) =
        case constsPool cls M.! off of
          (CMethod _ nt') -> ntSignature nt'
          (CIfaceMethod _ nt') -> ntSignature nt'
          _ -> error "methodType: fail :("
      rett = case returnt of
              Returns ft -> Just (fieldType2VarType ft)
              ReturnsVoid -> Nothing

  methodIsStatic :: Method Direct -> Bool
  methodIsStatic = S.member ACC_STATIC . methodAccessFlags

  methodArgs :: Num a => Method Direct -> a
  methodArgs meth = isStatic $ L.genericLength args
    where
      (MethodSignature args _) = methodSignature meth
      isStatic = if methodIsStatic meth then (+0) else (+1)

  -- TODO: Map to LLVM Type?
  fieldType2VarType :: FieldType -> LT.Type
  fieldType2VarType IntType = LT.i32
  fieldType2VarType CharByte = LT.i8
  fieldType2VarType BoolType = LT.i1
  fieldType2VarType FloatType = LT.float
  fieldType2VarType (ObjectType _) = LT.ptr
  fieldType2VarType (Array _ _) = LT.ptr -- fieldType2VarType ty -- TODO
  fieldType2VarType x = error $ "fieldType2VarType: " ++ show x

  -- tir = transform to IR
  -- TODO: Convert this to create LLVM...
  tir :: J.Instruction -> ParseState [LI.Instruction]
  -- tir ACONST_NULL = do apush LT.ptrNull; return []
  tir ICONST_M1 = tir (BIPUSH 0xff) -- (-1)
  tir ICONST_0 = tir (BIPUSH 0)
  tir ICONST_1 = tir (BIPUSH 1)
  tir ICONST_2 = tir (BIPUSH 2)
  tir ICONST_3 = tir (BIPUSH 3)
  tir ICONST_4 = tir (BIPUSH 4)
  tir ICONST_5 = tir (BIPUSH 5)
  tir (BIPUSH x) = do apush $ LO.ConstantOperand $ int x; return []
  tir (SIPUSH x) = do apush $ LO.ConstantOperand $ int x; return []
  tir FCONST_0 =  do apush $ LO.ConstantOperand $ LC.Float 0; return []
  tir FCONST_1 =  do apush $ LO.ConstantOperand $ LC.Float 1; return []
  tir FCONST_2 =  do apush $ LO.ConstantOperand $ LC.Float 3; return []
  tir (ILOAD_ x) = tir (ILOAD (imm2num x))
  tir (ILOAD x) = tirLoad x LT.i32 -- tirLoad' x LT.i32; return []
  tir (IINC x con) = do
    tirLoad' x LT.i32
    y <- apop
    nv <- newvar LT.i32
    apush nv
    storeinsn <- tirStore x LT.i32
    error "Not Supported"
    -- return $ IROp Add nv y (LT.i32Value (w8Toi32 con)) : storeinsn
  tir (ALOAD_ x) = tir (ALOAD (imm2num x))
  tir (ALOAD x) = tirLoad x LT.ptr
  tir (FLOAD_ x) = tir (FLOAD (imm2num x))
  tir (FLOAD x) = tirLoad x LT.float
  tir (ISTORE_ x) = tir (ISTORE (imm2num x))
  tir (ISTORE y) = tirStore y LT.i32
  tir (FSTORE_ y) = tir (FSTORE (imm2num y))
  tir (FSTORE y) = tirStore y LT.float
  tir (ASTORE_ x) = tir (ASTORE (imm2num x))
  tir (ASTORE x) = tirStore x LT.ptr
  tir (PUTFIELD x) = do
    error "Not Supported"
    -- src <- apop
    -- obj <- apop
    -- unless (LT.ptr == varType obj) $ error "putfield: type mismatch"
    -- cls <- classf <$> get
    -- unless (fieldType cls x == varType src) $ error "putfield: type mismatch2"
    -- return [IRStore (RTPool x) obj src]
  tir (GETFIELD x) = do
    error "Not Supported"
    -- obj <- apop
    -- unless (LT.ptr == varType obj) $ error "getfield: type mismatch"
    -- cls <- classf <$> get
    -- nv <- newvar (fieldType cls x)
    -- apush nv
    -- return [IRLoad (RTPool x) obj nv]
  tir (GETSTATIC x) = do
    error "Not Supported"
    -- cls <- classf <$> get
    -- nv <- newvar (fieldType cls x)
    -- apush nv
    -- return [IRLoad (RTPool x) LT.ptrNull nv]
  tir (PUTSTATIC x) = do
    error "Not Supported"
    -- y <- apop
    -- return [IRStore (RTPool x) LT.ptrNull y]
  tir (LDC1 x) = tir (LDC2 (fromIntegral x))
  tir (LDC2 x) = do
    cls <- classf <$> get
    let valuetype = case constsPool cls M.! x of
              (CString _) -> LT.ptr
              (CInteger _) -> LT.i32
              e -> error $ "tir: LDCI... missing impl.: " ++ show e
    nv <- newvar valuetype
    apush nv
    error "Not Supported..."
    -- return [IRLoad (RTPool x) LT.ptrNull nv]
  tir (NEW x) = do
    error "Not Supported"
    -- nv <- newvar LT.ptr
    -- apush nv
    -- return [IRLoad (RTPoolCall x []) LT.ptrNull nv]
  tir (ANEWARRAY _) = tirArray ReferenceType 10 -- for int. TODO?
  tir (NEWARRAY w8) = tirArray PrimitiveType w8
  tir ARRAYLENGTH = do
    error "Not Supported"
    -- array <- apop
    -- when (varType array /= LT.ptr) $ error "tir: arraylength: type mismatch"
    -- nv <- newvar LT.i32
    -- apush nv
    -- return [IRLoad RTArrayLength array nv]
  tir AALOAD = tirArrayLoad LT.ptr Nothing
  tir IALOAD = tirArrayLoad LT.i32 Nothing
  tir CALOAD = tirArrayLoad LT.i32 (Just 0xff)
  tir AASTORE = tirArrayStore LT.ptr Nothing
  tir IASTORE = tirArrayStore LT.i32 Nothing
  tir CASTORE = tirArrayStore LT.i32 (Just 0xff)
  tir DUP = do
    error "Not Supported"
    -- x <- apop
    -- apush x
    -- nv <- newvar (varType x)
    -- apush nv
    -- return [IROp Add nv x (LT.i32Value 0)]
  tir DUP_X1 = do
    error "Not Supported"
    -- v1 <- apop; v2 <- apop
    -- nv <- newvar (varType v1)
    -- apush nv
    -- apush v2; apush v1
    -- return [IROp Add nv v1 (LT.i32Value 0)]
  tir DUP_X2 = do
    -- WARNING: different behaviour for LONG & DOUBLE!!
    -- see, category 2 computational type (§2.11.1).
    error "Not Supported"
    -- v1 <- apop; v2 <- apop; v3 <- apop
    -- nv <- newvar (varType v1)
    -- apush nv
    -- apush v3; apush v2; apush v1
    -- return [IROp Add nv v1 (LT.i32Value 0)]
  tir POP = do apop; return []
  tir IADD = tirOpInt LI.Add LT.i32
  tir ISUB = tirOpInt LI.Sub LT.i32
  tir INEG = do
    x <- apop
    apush (LO.ConstantOperand $ int 0)
    apush x
    tirOpInt LI.Sub LT.i32
  tir IMUL = tirOpInt LI.Mul LT.i32
  tir IDIV = tirOpInt LI.SDiv LT.i32
  tir IREM = tirOpInt LI.SRem LT.i32
  tir IAND = tirOpInt LI.And LT.i32
  tir IOR = tirOpInt LI.Or LT.i32
  tir IXOR = tirOpInt LI.Xor LT.i32
  tir IUSHR = tirOpInt LI.LShr LT.i32
  tir ISHR = tirOpInt LI.AShr LT.i32
  tir ISHL = tirOpInt LI.Shl LT.i32
  tir FADD = tirOpInt LI.Add LT.float
  tir I2C = do
    error "Not Supported"
    -- x <- apop
    -- when (varType x /= LT.i32) $ error "tir: i2c: type mismatch"
    -- nv <- newvar LT.i32
    -- apush nv
    -- return [IROp And nv x (LT.i32Value 0xff)]
  -- tir (INVOKESTATIC ident) = tirInvoke CallStatic ident
  -- tir (INVOKESPECIAL ident) = tirInvoke CallSpecial ident
  -- tir (INVOKEVIRTUAL ident) = tirInvoke CallVirtual ident
  -- tir (INVOKEINTERFACE ident _) = tirInvoke CallInterface ident
  -- tir i@(CHECKCAST _) = do
  --   y <- apop
  --   apush y
  --   return [IRMisc1 i y]
  -- tir i@(INSTANCEOF _) = do
  --   y <- apop
  --   nv <- newvar LT.i32
  --   apush nv
  --   return [IRMisc2 i nv y]
  -- tir i@ATHROW = do
  --   y <- apop
  --   return [IRMisc1 i y]
  -- tir MONITORENTER = do -- TODO: stub!
  --   apop; return []
  -- tir MONITOREXIT = do -- TODO: stub!
  --   apop; return []
  -- tir x = error $ "tir: " ++ show x

  tirArray :: MateObjType -> Word8 -> ParseState ()
  tirArray objtype w8 = do
    error "Not Supported"
    -- len <- apop
    -- when (varType len /= LT.i32) $ error "tir: tirArray: type mismatch"
    -- nv <- newvar LT.ptr
    -- apush nv
    -- return [IRLoad (RTArrayNew w8 objtype [] len) LT.ptrNull nv]
  --
  -- tirArrayLoad :: VarType -> Maybe Int32 {- Mask -} -> ParseState [MateIR Var O O]
  -- tirArrayLoad t mask = do
  --   idx <- apop
  --   array <- apop
  --   when (varType array /= LT.ptr) $ error "tir: aaload: type mismatch1"
  --   when (varType idx /= LT.i32) $ error "tir: aaload: type mismatch2"
  --   nv <- newvar t
  --   apush nv
  --   case mask of
  --     Just m -> do
  --       _ <- apop
  --       nv' <- newvar LT.i32
  --       apush nv'
  --       return [ IRLoad (RTArrayIndex idx t) array nv
  --              , IROp And nv' nv (LT.i32Value m)]
  --     _ -> return [IRLoad (RTArrayIndex idx t) array nv]
  --
  -- tirArrayStore :: VarType -> Maybe Int32 {- Mask -} -> ParseState [MateIR Var O O]
  -- tirArrayStore t mask = do
  --   value <- apop
  --   idx <-   apop
  --   array <- apop
  --   when (varType array /= LT.ptr) $
  --     error $ "tir: tirArrayStore: type mismatch1: " ++ show (varType array)
  --   when (varType idx /= LT.i32) $
  --     error $ "tir: tirArrayStore: type mismatch2: " ++ show (varType idx)
  --   when (varType value /= t) $
  --     error $ "tir: tirArrayStore: type mismatch3: " ++ show t
  --   case mask of
  --     Just m -> do
  --       nv <- newvar LT.i32
  --       return [ IROp And nv value (LT.i32Value m)
  --              , IRStore (RTArrayIndex idx t) array nv ]
  --     _ -> return [IRStore (RTArrayIndex idx t) array value]

  -- tirInvoke :: CallType -> Word16 -> ParseState [MateIR Var O O]
  -- tirInvoke ct ident = do
  --   cls <- classf <$> get
  --   let (varts, mret) = methodType (ct /= CallStatic) cls ident
  --   pushes <- tracePipe (printf "tirInvoke: varts: %s returns %s\n" (show varts) (show mret)) $
  --             forM (reverse $ zip varts [0..]) $ \(x, nr) -> do
  --     y <- apop
  --     unless (x == varType y) $ error "invoke: type mismatch"
  --     case x of
  --       LT.i32 -> return $ IRPush nr y
  --       LT.ptr -> return $ IRPush nr y
  --       LT.float -> do
  --         let nr8 = fromIntegral nr
  --         let nri = fromIntegral nr
  --         let assign = preFloats !! nri
  --         modify (\s -> s { preRegs = M.insert
  --                                     (VR assign LT.float)
  --                                     (HFReg $ XMMReg nr8)
  --                                     (preRegs s) })
  --         return $ IROp Add (VReg (VR assign x)) y (LT.floatValue 0) -- mov
  --   (targetreg, maybemov) <- case mret of
  --     Just x -> do
  --       let prereg = case x of
  --                       LT.i32 -> preeax
  --                       LT.float -> prexmm7
  --                       LT.ptr -> preeax
  --       let nv = VReg (VR prereg x)
  --       movtarget <- newvar x
  --       tracePipe (printf "return: %s@%s\n" (show prereg) (show x)) $
  --         apush movtarget
  --       return (Just nv, Just $ IROp Add movtarget nv (LT.i32Value 0))
  --     Nothing -> return (Nothing, Nothing)
  --   let r =  IRPrep SaveRegs [] : pushes ++
  --           [IRInvoke (RTPoolCall ident []) targetreg ct, IRPrep RestoreRegs []]
  --   case maybemov of
  --     Nothing -> return r
  --     Just m -> return $ r ++ [m]

  -- maybeArgument :: Word8 -> VarType -> ParseState Var
  -- maybeArgument x t = do
  --   meth <- method <$> get
  --   let genVReg :: (Disp -> HVarX86) -> VRegNR
  --               -> Word8 -> VarType
  --               -> (VirtualReg, HVarX86)
  --       genVReg constructor a w8 t' =
  --         (VR a t'
  --         ,constructor . Disp . (+ (3 * ptrSize)) . fromIntegral $ (ptrSize * w8))
  --   if x < methodArgs meth
  --     then do
  --       ((tup'k, tup'v), assign') <- case t of
  --        LT.float -> do
  --          let assign = preFloats !! fromIntegral x
  --          let tup = (VR assign LT.float, HFReg . XMMReg . fromIntegral $ x)
  --          return (tup, assign)
  --        _ -> do
  --          let assign = preArgs !! fromIntegral x
  --          let constr = case t of
  --                   LT.ptr -> SpillIReg
  --                   LT.i32 -> SpillIReg
  --                   LT.float -> error "can't happen"
  --          let tup = genVReg constr assign x LT.i32
  --          return (tup, assign)
  --       modify (\s -> s { preRegs = M.insert tup'k tup'v (preRegs s) })
  --       return $ VReg (VR assign' t)
  --     else return $ VReg (VR (fromIntegral x) t)

  -- Generate a named local variable name for each possible local variable
  setupLocals :: ParseState ()
  setupLocals = do
    -- For each maximum locals, generate a name for it in advance
    maxLocals <- J.codeMaxLocals <$> gets method
    names <- forM [0..maxLocals-1] $ \i -> do
      let name = LN.mkName ("L" ++ show i)
      appendInstruction $ name LI.:= alloca LT.i32
      return name
    modify $ \s -> s { localVariableNames = names }

  -- Obtains the local variable at requested index
  getLocal :: Integral a => a -> CFG LN.Name
  getLocal idx = (!! fromIntegral idx) <$> gets localVariableNames

  tirLoad' :: Word8 -> LT.Type -> ParseState ()
  tirLoad' x t = do
    vreg <- maybeArgument x t
    apush vreg

  nul :: VarType -> Var
  nul t = case t of
    LT.i32 -> LT.i32Value 0
    LT.float -> LT.floatValue 0
    LT.ptr -> LT.ptrNull

  tirLoad :: Word8 -> VarType -> ParseState [MateIR Var O O]
  tirLoad x t = do
    tirLoad' x t
    vreg <- apop
    nv <- newvar t
    apush nv
    return [IROp Add nv vreg (nul t)]


  tirStore :: Word8 -> VarType -> ParseState[MateIR Var O O]
  tirStore w8 t = do
    x <- apop
    unless (t == varType x) $ error "tirStore: type mismatch"
    vreg <- maybeArgument w8 t
    return [IROp Add vreg x (nul t)]

  tirOpInt :: OpType -> VarType -> ParseState [MateIR Var O O]
  tirOpInt op t = do
    x <- apop; y <- apop
    nv <- newvar t; apush nv
    unless (t == varType x && t == varType y) $ error "tirOpInt: type mismatch"
    return [IROp op nv x y]

  newvar :: VarType -> ParseState Var
  newvar t = do
    sims <- get
    put $ sims { regcnt = regcnt sims + 1 }
    return $ VReg (VR (regcnt sims) t)

  apush :: Var -> ParseState ()
  apush x = do
    s <- stack <$> get
    sims <- get
    put $ sims { stack = x : s }

  apop :: ParseState Var
  apop = do
    simstack <- stack <$> get
    when (null simstack) $ error "apop: stack is empty"
    (s:ss) <- stack <$> get
    modify (\m -> m { stack = ss })
    return s
