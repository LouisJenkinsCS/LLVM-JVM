{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module MateVMRuntime.MethodPool
  ( lookupMethodEntry
  , executeFuncPtr
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Maybe (fromJust)

import Foreign
import Foreign.C.Types
import Unsafe.Coerce

import JVM.ClassFile
import JVM.Assembler

import MateVMRuntime.Debug
import MateVMRuntime.Types

import MateVMRuntime.NativeSizes
import MateVMRuntime.Utilities
import MateVMRuntime.ClassPool
import MateVMRuntime.NativeMethods

import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as LG
import qualified LLVM.AST.Linkage as LL
import qualified LLVM.ExecutionEngine as EE
import qualified LLVM.AST.Constant as LC
import LLVM.Context
import LLVM.CodeModel
import LLVM.Target
import qualified LLVM.Module as Mod
import LLVM.PassManager
import LLVMFrontend.Helpers
import LLVMFrontend.MkGraph
import LLVM.Transforms


foreign import ccall "dynamic"
   code_void :: FunPtr (IO ()) -> IO ()

foreign import ccall "dynamic"
 code_int :: FunPtr (IO Int) -> IO Int

lookupMethodEntry:: MethodInfo -> IO CPtrdiff
lookupMethodEntry mi@(MethodInfo method cm sig) = do
  mmap <- getMethodMap
  case M.lookup mi mmap of
    Nothing -> do
      cls <- getClassFile cm
      printfMp $ printf "getMethodEntry: no method \"%s\" found. compile it\n" (show mi)
      mm <- lookupMethodRecursive method sig [] cls
      case mm of
        Just (mm', clsnames, cls') -> do
            let flags = methodAccessFlags mm'
            nf <- if S.member ACC_NATIVE flags
              then nativeAddr (toString cm) (toString method) (toString $ encode sig)
              else do -- plain java method
                entry <- compile $ MethodInfo method (thisClass cls') sig
                insertCompiledMethod entry mi clsnames
                return $ fromIntegral entry
            setMethodMap $ M.insert mi nf mmap
            return $ fromIntegral nf
        Nothing -> error $ printf "\"%s\" not found. abort" (toString method)
    Just w32 -> return $ fromIntegral w32

lookupMethodRecursive :: B.ByteString -> MethodSignature -> [B.ByteString] -> Class Direct
                         -> IO (Maybe (Method Direct, [B.ByteString], Class Direct))
lookupMethodRecursive name sig clsnames cls = do
  printfCp $ printf "looking @ %s\n" (toString thisname)
  case res of
    Just x -> return $ Just (x, nextclsn, cls)
    Nothing -> if thisname == "java/lang/Object"
      then return Nothing
      else do
        supercl <- getClassFile (superClass cls)
        lookupMethodRecursive name sig nextclsn supercl
  where
    res = lookupMethodWithSig name sig cls
    thisname = thisClass cls
    nextclsn :: [B.ByteString]
    nextclsn = thisname:clsnames

insertCompiledMethod :: NativeWord -> MethodInfo -> [B.ByteString] -> IO ()
insertCompiledMethod entry (MethodInfo mmname _ msig) clsnames = do
  mmap <- getMethodMap
  let newmap = foldr
                  (\i -> M.insert (MethodInfo mmname i msig) entry)
                  M.empty
                  clsnames
  setMethodMap $ mmap `M.union` newmap

-- Create dummy module to be invoked by JIT
initModule :: AST.Module
initModule = AST.defaultModule { AST.moduleName = "dummy" }

mkName' :: B.ByteString -> AST.Name
mkName' = AST.mkName . map unsafeCoerce . B.unpack

parseFields :: Class Direct -> [AST.Definition]
parseFields cls = 
  flip map (classFields cls) $ \field -> 
    let nameType = fieldNameType field in
      AST.GlobalDefinition $ LG.globalVariableDefaults { 
        LG.name = mkName' $ ntName nameType,
        LG.type' = jvm2llvmType $ ntSignature nameType,
        LG.linkage = LL.Private,
        LG.initializer = Just $ LC.Int 32 0
      }

-- Stubbed for now...
-- TODO: Do LLVM
compileMethod :: B.ByteString -> MethodSignature -> Class Direct -> IO (a,b)
compileMethod name sig cls = do
  let meth = fromJust $ lookupMethod name cls
  let code = M.fromList (arlist (methodAttributes meth)) M.! "Code"

  cfg <- pipeline cls meth (codeInstructions . decodeMethod $ code)
  -- cfg <- parseCFG (decodeMethod code)
  let mod = AST.defaultModule { AST.moduleDefinitions = parseFields cls ++ [defineFn (returnType sig) "main" (M.elems $ basicBlocks cfg)], AST.moduleName = "Dummy" }
  ast <- compileMethod' mod
  error . show $ ast

  error "JIT Compilation not implemented...\n"

compileMethod' mod =
  -- Create a context used within this scope, which contains all LLVM-specific information
  withContext $ \c ->
    -- Create an execution engine within this scope...
    EE.withMCJIT c (Just 0) Nothing Nothing Nothing $ \ee ->
      -- Create an LLVM module from the AST generated...
      Mod.withModuleFromAST c mod $ \m ->
        -- Make a simple optimization pass
        withPassManager defaultPassSetSpec { transforms = [PromoteMemoryToRegister, JumpThreading] } $ \pm -> do
          optmod <- Mod.moduleAST m -- Optimized-copy of module
          s <- Mod.moduleLLVMAssembly m -- Convert from module to assembly
          printfJit $ "~~~Original...~~~\n" ++ C8.unpack s
          runPassManager pm m
          s' <- Mod.moduleLLVMAssembly m -- Convert from module to assembly
          printfJit $ "~~~Optimized...~~~\n" ++ C8.unpack s'

          -- Actually execute module... execution engine is modified to contain the
          -- actual code...
          EE.withModuleInEngine ee m $ \ee' -> do
            -- Find and execute main method...
            mainfn <- EE.getFunction ee' "main"
            case mainfn of
              Just fn -> do
                result <- code_int (castFunPtr fn :: FunPtr (IO Int))
                printfJit . show $ result
                
              Nothing -> return ()

          -- Optimized module used only in next pass...
          return optmod

compile :: MethodInfo -> IO NativeWord
compile methodinfo = time (printf "compile \"%s\"" $ toString $ methName methodinfo) $ do
  tmap <- getTrapMap

  cls <- getClassFile (methClassName methodinfo)
  printfJit $ printf "emit code of \"%s\" from \"%s\":\n"
               (toString $ methName methodinfo)
               (toString $ methClassName methodinfo)
  (entry, new_trapmap) <- compileMethod (methName methodinfo)
                                     (methSignature methodinfo)
                                     cls
  setTrapMap $ tmap `M.union` new_trapmap -- prefers elements in tmap
  printfJit $ printf "generated code of \"%s\" @ \"%s\" from \"%s\". DONE\n"
               (toString $ methName methodinfo)
               (show $ methSignature methodinfo)
               (toString $ methClassName methodinfo)

  -- UNCOMMENT NEXT LINES FOR GDB FUN
  -- if (toString $ methName methodinfo) == "thejavamethodIwant2debug"
  --   then putStrLn "press CTRL+C now for setting a breakpoint. then `c' and ENTER for continue" >> getLine
  --   else return "foo"
  -- (1) build a debug build (see HACKING) and execute `make tests/Fib.gdb'
  --     for example, where the suffix is important
  -- (2) on getLine, press CTRL+C
  -- (3) `br *0x<addr>'; obtain the address from the disasm above
  -- (4) `cont' and press enter
  return entry

executeFuncPtr :: NativeWord -> IO ()
executeFuncPtr entry =
  code_void ((castPtrToFunPtr $ intPtrToPtr $ fromIntegral entry) :: FunPtr (IO ()))
