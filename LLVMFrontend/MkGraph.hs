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

  -- import Debug.Trace

  data ParseState' = ParseState'
    { labels :: M.Map Int32 Label {- store offset -> label -}

    , nextTargets :: [Label] {- only valid per block. needed for block fixups -}
    , blockInterfaces :: M.Map Label [Var] {- store interface map for basic blocks -}
    , blockEntries :: S.Set Int32 {- store block entries (data gained from first pass) -}

    , pcOffset :: Int32 {- programm counter -}
    , stack :: [Var] {- simulation stack -}
    , regcnt :: VRegNR {- counter for virtual registers -}
    , classf :: Class Direct {- reference to class of processed method -}
    , method :: Method Direct {- reference to processed method -}
    , preRegs :: RegMapping

    , instructions :: [J.Instruction] {- instructions to process -}
    -- Note: The ExceptionMap is a type-alias for the IntervalMap, which maps an
    -- interval (I.E: [1,10] representing all numbers between 1 and 10) to its
    -- exception handler. This allows us to search whether or not a particular program
    -- counter has an exception handler within amortized O(log n) time.
    , exceptionMap :: ExceptionMap Int32 {- map of try-blocks, with references to handler -}
    , handlerStarts :: S.Set Int32 {- set of handler starts -}
    }

  type ParseState a = StateT ParseState' SimpleUniqueMonad a

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


  mkMethod :: Graph (MateIR Var) C C -> ParseState (Graph (MateIR Var) O C)
  mkMethod g = do
    hs <- handlerStarts <$> get
    -- Create labels for each exception handler, as well as for the entry block.
    entryseq <- mkLast <$> IRExHandler <$> mapM addLabel (S.toList hs ++ [0])
    return $ entryseq |*><*| g

  mkBlocks :: ParseState [Graph (MateIR Var) C C]
  mkBlocks = do
    pc <- pcOffset <$> get
    entries <- blockEntries <$> get
    jvminsn <- instructions <$> get

    if null jvminsn
      then return []
      else if S.member pc entries
        then do
          g <- mkBlock
          gs <- mkBlocks
          return $ g : gs
        else error $ "mkBlocks: something wrong here. pc: " ++ show pc ++
                     "\ninsn: " ++ show jvminsn

  mkBlock :: ParseState (Graph (MateIR Var) C C)
  mkBlock = do
    modify (\s -> s { nextTargets = [] }) -- Clear state
    handlermap <- exceptionMap <$> get
    pc <- pcOffset <$> get
    l <- addLabel pc
    -- push JRef for Exceptionblock, which is passed via %eax
    isExceptionHandler <- S.member pc <$> handlerStarts <$> get
    handlerStart <- if isExceptionHandler
      then do
        apush (VReg (VR preeax JRef))
        return $ Just $ fromIntegral pc
      else return Nothing

    -- Compute the exception handlers that we are apart of. As there cna be nested
    -- exception handlers/try-catch blocks, we end up with a list of the ones we
    -- are contained in.
    let extable = map (second fromIntegral)
                  $ concatMap snd
                  $ handlermap `IM.containing` pc
    let f' = IRLabel l extable handlerStart
    -- fixup block boundaries
    be <- -- trace (printf "pc: %d\nhstart: %s\nextable: %s\n" pc (show handlerStart) (show extable)) $
          M.lookup l <$> blockInterfaces <$> get
    fixup <- case be of
      Nothing -> return []
      Just ts -> forM ts $ \x -> do
                   nv <- newvar $ varType x
                   apush nv
                   return $ IROp Add nv x (nul (varType x))
    (ms', l') <- toMid
    return $ mkFirst f' <*> mkMiddles (fixup ++ ms') <*> mkLast l'

  -- Add a label for the provided program counter, if not already.
  addLabel :: Int32 -> ParseState Label
  addLabel boff = do
    lmap <- labels <$> get
    if M.member boff lmap
      then return $ lmap M.! boff -- Already present
      else do
        -- Construct a new label. We also create an outgoing edge to the label,
        -- as otherwise it would violate the basic block invariant.
        label <- lift freshLabel
        modify (\s -> s { labels = M.insert boff label (labels s)
                        , nextTargets = label : nextTargets s })
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

  toMid :: ParseState ([MateIR Var O O], MateIR Var O C)
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
      toLast :: J.Instruction -> ParseState ([MateIR Var O O], MateIR Var O C)
      toLast ins = do
        pc <- pcOffset <$> get
        -- Handle if-else statement, where we have two outgoing edges from the
        -- conditionally-executed basic block, and to the end of the basic block.
        let ifstuff jcmp rel op1 op2 = do
              truejmp <- addLabel (pc + w16Toi32 rel)
              falsejmp <- addLabel (pc + insnLength ins)
              return ([], IRIfElse jcmp op1 op2 truejmp falsejmp)
        -- Handle switch statements, adding outgoing edges to each case statement.
        let switchins def switch' = do
              y <- apop
              switch <- forM switch' $ \(v, o) -> do
                offset <- addLabel $ pc + fromIntegral o
                return (Just (w32Toi32 v), offset)
              defcase <- addLabel $ pc + fromIntegral def
              return ([], IRSwitch y $ switch ++ [(Nothing, defcase)])
        (ret1, ret2) <- case ins of
          RETURN -> return ([], IRReturn Nothing)
          ARETURN -> returnSomething JRef
          IRETURN -> returnSomething JInt
          LRETURN -> error "toLast: LReturn"
          FRETURN -> returnSomething JFloat
          DRETURN -> error "toLast: DReturn"
          (IF jcmp rel) -> do
            let op1 = JIntValue 0
            op2 <- apop
            unless (varType op2 == JInt) $ error "toLast IF: type mismatch"
            ifstuff jcmp rel op1 op2
          (IFNULL rel) -> do
            op1 <- apop
            unless (varType op1 == JRef) $ error "toLast IFNULL: type mismatch"
            ifstuff C_EQ rel op1 (JIntValue 0)
          (IFNONNULL rel) -> do
            op1 <- apop
            unless (varType op1 == JRef) $ error "toLast IFNONNULL: type mismatch"
            ifstuff C_NE rel op1 (JIntValue 0)
          (IF_ICMP jcmp rel) -> do
            op1 <- apop
            op2 <- apop
            unless (varType op1 == varType op2) $ error "toLast IF_ICMP: type mismatch"
            ifstuff jcmp rel op1 op2
          (IF_ACMP jcmp rel) -> do
            op1 <- apop
            op2 <- apop
            unless (varType op1 == varType op2) $ error "toLast IF_ACMP: type mismatch"
            ifstuff jcmp rel op1 op2
          (GOTO rel) -> do
            jump <- addLabel (pc + w16Toi32 rel)
            return ([], IRJump jump)
          TABLESWITCH _ def low high offs -> switchins def $ zip [low..high] offs
          LOOKUPSWITCH _ def _ switch -> switchins def switch
          _ -> do -- fallthrough case
            next <- addLabel (pc + insnLength ins)
            insIR <- tir ins
            return (insIR, IRJump next)
        fixups <- handleBlockEnd
        return (ret1 ++ fixups, ret2)
        where
          -- Return the top operand (after performing type-checking)
          returnSomething t = do
            r <- apop
            unless (varType r == t) $ error "toLast return: type mismatch"
            return ([], IRReturn $ Just r)

  handleBlockEnd :: ParseState [MateIR Var O O]
  handleBlockEnd = do
    st <- get
    let len = L.genericLength $ stack st
    if len > 0
      then
        forM [600000 .. (600000 + len - 1)] $ \r -> do
          x <- apop
          let vreg = VReg (VR r (varType x))
          targets <- nextTargets <$> get
          forM_ targets $ \t -> do
            be <- fromMaybe [] <$> M.lookup t <$> blockInterfaces <$> get
            modify (\s -> s { blockInterfaces = M.insert t (vreg:be) (blockInterfaces s)})
          return (IROp Add vreg x (nul (varType x)))
      else return []

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

  fieldType :: Class Direct -> Word16 -> VarType
  fieldType cls off = fieldType2VarType $ ntSignature nt
    where nt = case constsPool cls M.! off of
                  (CField _ nt') -> nt'
                  _ -> error "fieldType: fail :("

  methodType :: Bool -> Class Direct -> Word16 -> ([VarType], Maybe VarType)
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
  fieldType2VarType :: FieldType -> VarType
  fieldType2VarType IntType = JInt
  fieldType2VarType CharByte = JInt
  fieldType2VarType BoolType = JInt
  fieldType2VarType FloatType = JFloat
  fieldType2VarType (ObjectType _) = JRef
  fieldType2VarType (Array _ _) = JRef -- fieldType2VarType ty -- TODO
  fieldType2VarType x = error $ "fieldType2VarType: " ++ show x

  -- tir = transform to IR
  -- TODO: Convert this to create LLVM...
  tir :: J.Instruction -> ParseState [MateIR Var O O]
  tir ACONST_NULL = do apush JRefNull; return []
  tir ICONST_M1 = tir (BIPUSH 0xff) -- (-1)
  tir ICONST_0 = tir (BIPUSH 0)
  tir ICONST_1 = tir (BIPUSH 1)
  tir ICONST_2 = tir (BIPUSH 2)
  tir ICONST_3 = tir (BIPUSH 3)
  tir ICONST_4 = tir (BIPUSH 4)
  tir ICONST_5 = tir (BIPUSH 5)
  tir (BIPUSH x) = do apush $ JIntValue (w8Toi32 x); return []
  tir (SIPUSH x) = do apush $ JIntValue (w16Toi32 x); return []
  tir FCONST_0 =  do apush $ JFloatValue 0; return []
  tir FCONST_1 =  do apush $ JFloatValue 1; return []
  tir FCONST_2 =  do apush $ JFloatValue 3; return []
  tir (ILOAD_ x) = tir (ILOAD (imm2num x))
  tir (ILOAD x) = do tirLoad x JInt -- tirLoad' x JInt; return []
  tir (IINC x con) = do
    tirLoad' x JInt
    y <- apop
    nv <- newvar JInt
    apush nv
    storeinsn <- tirStore x JInt
    return $ IROp Add nv y (JIntValue (w8Toi32 con)) : storeinsn
  tir (ALOAD_ x) = tir (ALOAD (imm2num x))
  tir (ALOAD x) = tirLoad x JRef
  tir (FLOAD_ x) = tir (FLOAD (imm2num x))
  tir (FLOAD x) = tirLoad x JFloat
  tir (ISTORE_ x) = tir (ISTORE (imm2num x))
  tir (ISTORE y) = tirStore y JInt
  tir (FSTORE_ y) = tir (FSTORE (imm2num y))
  tir (FSTORE y) = tirStore y JFloat
  tir (ASTORE_ x) = tir (ASTORE (imm2num x))
  tir (ASTORE x) = tirStore x JRef
  tir (PUTFIELD x) = do
    src <- apop
    obj <- apop
    unless (JRef == varType obj) $ error "putfield: type mismatch"
    cls <- classf <$> get
    unless (fieldType cls x == varType src) $ error "putfield: type mismatch2"
    return [IRStore (RTPool x) obj src]
  tir (GETFIELD x) = do
    obj <- apop
    unless (JRef == varType obj) $ error "getfield: type mismatch"
    cls <- classf <$> get
    nv <- newvar (fieldType cls x)
    apush nv
    return [IRLoad (RTPool x) obj nv]
  tir (GETSTATIC x) = do
    cls <- classf <$> get
    nv <- newvar (fieldType cls x)
    apush nv
    return [IRLoad (RTPool x) JRefNull nv]
  tir (PUTSTATIC x) = do
    y <- apop
    return [IRStore (RTPool x) JRefNull y]
  tir (LDC1 x) = tir (LDC2 (fromIntegral x))
  tir (LDC2 x) = do
    cls <- classf <$> get
    let valuetype = case constsPool cls M.! x of
              (CString _) -> JRef
              (CInteger _) -> JInt
              e -> error $ "tir: LDCI... missing impl.: " ++ show e
    nv <- newvar valuetype
    apush nv
    return [IRLoad (RTPool x) JRefNull nv]
  tir (NEW x) = do
    nv <- newvar JRef
    apush nv
    return [IRLoad (RTPoolCall x []) JRefNull nv]
  tir (ANEWARRAY _) = tirArray ReferenceType 10 -- for int. TODO?
  tir (NEWARRAY w8) = tirArray PrimitiveType w8
  tir ARRAYLENGTH = do
    array <- apop
    when (varType array /= JRef) $ error "tir: arraylength: type mismatch"
    nv <- newvar JInt
    apush nv
    return [IRLoad RTArrayLength array nv]
  tir AALOAD = tirArrayLoad JRef Nothing
  tir IALOAD = tirArrayLoad JInt Nothing
  tir CALOAD = tirArrayLoad JInt (Just 0xff)
  tir AASTORE = tirArrayStore JRef Nothing
  tir IASTORE = tirArrayStore JInt Nothing
  tir CASTORE = tirArrayStore JInt (Just 0xff)
  tir DUP = do
    x <- apop
    apush x
    nv <- newvar (varType x)
    apush nv
    return [IROp Add nv x (JIntValue 0)]
  tir DUP_X1 = do
    v1 <- apop; v2 <- apop
    nv <- newvar (varType v1)
    apush nv
    apush v2; apush v1
    return [IROp Add nv v1 (JIntValue 0)]
  tir DUP_X2 = do
    -- WARNING: different behaviour for LONG & DOUBLE!!
    -- see, category 2 computational type (§2.11.1).
    v1 <- apop; v2 <- apop; v3 <- apop
    nv <- newvar (varType v1)
    apush nv
    apush v3; apush v2; apush v1
    return [IROp Add nv v1 (JIntValue 0)]
  tir POP = do apop; return []
  tir IADD = tirOpInt Add JInt
  tir ISUB = tirOpInt Sub JInt
  tir INEG = do
    x <- apop
    apush (JIntValue 0)
    apush x
    tirOpInt Sub JInt
  tir IMUL = tirOpInt Mul JInt
  tir IDIV = tirOpInt Div JInt
  tir IREM = tirOpInt Rem JInt
  tir IAND = tirOpInt And JInt
  tir IOR = tirOpInt Or JInt
  tir IXOR = tirOpInt Xor JInt
  tir IUSHR = tirOpInt ShiftRightLogical JInt
  tir ISHR = tirOpInt ShiftRightArth JInt
  tir ISHL = tirOpInt ShiftLeft JInt
  tir FADD = tirOpInt Add JFloat
  tir I2C = do
    x <- apop
    when (varType x /= JInt) $ error "tir: i2c: type mismatch"
    nv <- newvar JInt
    apush nv
    return [IROp And nv x (JIntValue 0xff)]
  tir (INVOKESTATIC ident) = tirInvoke CallStatic ident
  tir (INVOKESPECIAL ident) = tirInvoke CallSpecial ident
  tir (INVOKEVIRTUAL ident) = tirInvoke CallVirtual ident
  tir (INVOKEINTERFACE ident _) = tirInvoke CallInterface ident
  tir i@(CHECKCAST _) = do
    y <- apop
    apush y
    return [IRMisc1 i y]
  tir i@(INSTANCEOF _) = do
    y <- apop
    nv <- newvar JInt
    apush nv
    return [IRMisc2 i nv y]
  tir i@ATHROW = do
    y <- apop
    return [IRMisc1 i y]
  tir MONITORENTER = do -- TODO: stub!
    apop; return []
  tir MONITOREXIT = do -- TODO: stub!
    apop; return []
  tir x = error $ "tir: " ++ show x

  tirArray :: MateObjType -> Word8 -> ParseState [MateIR Var O O]
  tirArray objtype w8 = do
    len <- apop
    when (varType len /= JInt) $ error "tir: tirArray: type mismatch"
    nv <- newvar JRef
    apush nv
    return [IRLoad (RTArrayNew w8 objtype [] len) JRefNull nv]

  tirArrayLoad :: VarType -> Maybe Int32 {- Mask -} -> ParseState [MateIR Var O O]
  tirArrayLoad t mask = do
    idx <- apop
    array <- apop
    when (varType array /= JRef) $ error "tir: aaload: type mismatch1"
    when (varType idx /= JInt) $ error "tir: aaload: type mismatch2"
    nv <- newvar t
    apush nv
    case mask of
      Just m -> do
        _ <- apop
        nv' <- newvar JInt
        apush nv'
        return [ IRLoad (RTArrayIndex idx t) array nv
               , IROp And nv' nv (JIntValue m)]
      _ -> return [IRLoad (RTArrayIndex idx t) array nv]

  tirArrayStore :: VarType -> Maybe Int32 {- Mask -} -> ParseState [MateIR Var O O]
  tirArrayStore t mask = do
    value <- apop
    idx <-   apop
    array <- apop
    when (varType array /= JRef) $
      error $ "tir: tirArrayStore: type mismatch1: " ++ show (varType array)
    when (varType idx /= JInt) $
      error $ "tir: tirArrayStore: type mismatch2: " ++ show (varType idx)
    when (varType value /= t) $
      error $ "tir: tirArrayStore: type mismatch3: " ++ show t
    case mask of
      Just m -> do
        nv <- newvar JInt
        return [ IROp And nv value (JIntValue m)
               , IRStore (RTArrayIndex idx t) array nv ]
      _ -> return [IRStore (RTArrayIndex idx t) array value]

  tirInvoke :: CallType -> Word16 -> ParseState [MateIR Var O O]
  tirInvoke ct ident = do
    cls <- classf <$> get
    let (varts, mret) = methodType (ct /= CallStatic) cls ident
    pushes <- tracePipe (printf "tirInvoke: varts: %s returns %s\n" (show varts) (show mret)) $
              forM (reverse $ zip varts [0..]) $ \(x, nr) -> do
      y <- apop
      unless (x == varType y) $ error "invoke: type mismatch"
      case x of
        JInt -> return $ IRPush nr y
        JRef -> return $ IRPush nr y
        JFloat -> do
          let nr8 = fromIntegral nr
          let nri = fromIntegral nr
          let assign = preFloats !! nri
          modify (\s -> s { preRegs = M.insert
                                      (VR assign JFloat)
                                      (HFReg $ XMMReg nr8)
                                      (preRegs s) })
          return $ IROp Add (VReg (VR assign x)) y (JFloatValue 0) -- mov
    (targetreg, maybemov) <- case mret of
      Just x -> do
        let prereg = case x of
                        JInt -> preeax
                        JFloat -> prexmm7
                        JRef -> preeax
        let nv = VReg (VR prereg x)
        movtarget <- newvar x
        tracePipe (printf "return: %s@%s\n" (show prereg) (show x)) $
          apush movtarget
        return (Just nv, Just $ IROp Add movtarget nv (JIntValue 0))
      Nothing -> return (Nothing, Nothing)
    let r =  IRPrep SaveRegs [] : pushes ++
            [IRInvoke (RTPoolCall ident []) targetreg ct, IRPrep RestoreRegs []]
    case maybemov of
      Nothing -> return r
      Just m -> return $ r ++ [m]

  maybeArgument :: Word8 -> VarType -> ParseState Var
  maybeArgument x t = do
    meth <- method <$> get
    let genVReg :: (Disp -> HVarX86) -> VRegNR
                -> Word8 -> VarType
                -> (VirtualReg, HVarX86)
        genVReg constructor a w8 t' =
          (VR a t'
          ,constructor . Disp . (+ (3 * ptrSize)) . fromIntegral $ (ptrSize * w8))
    if x < methodArgs meth
      then do
        ((tup'k, tup'v), assign') <- case t of
         JFloat -> do
           let assign = preFloats !! fromIntegral x
           let tup = (VR assign JFloat, HFReg . XMMReg . fromIntegral $ x)
           return (tup, assign)
         _ -> do
           let assign = preArgs !! fromIntegral x
           let constr = case t of
                    JRef -> SpillIReg
                    JInt -> SpillIReg
                    JFloat -> error "can't happen"
           let tup = genVReg constr assign x JInt
           return (tup, assign)
        modify (\s -> s { preRegs = M.insert tup'k tup'v (preRegs s) })
        return $ VReg (VR assign' t)
      else return $ VReg (VR (fromIntegral x) t)


  tirLoad' :: Word8 -> VarType -> ParseState ()
  tirLoad' x t = do
    vreg <- maybeArgument x t
    apush vreg

  nul :: VarType -> Var
  nul t = case t of
    JInt -> JIntValue 0
    JFloat -> JFloatValue 0
    JRef -> JRefNull

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
