{-# LANGUAGE OverloadedStrings, FlexibleContexts, GADTs #-}
module MateVMRuntime.ClassPool (
  getClassInfo,
  getClassInfoNoInit,
  classLoaded,
  getClassFile,
  getMethodTable,
  getMethodTableNoInit,
  getMethodTableReverse,
  getObjectSize,
  getFieldCount,
  getStaticFieldTypes,
  getFieldTypes,
  getMethodOffset,
  getFieldOffset,
  getFieldSignatures,
  getStaticFieldAddr,
  getInterfaceMethodOffset,
  addClassPath,
  addClassPathJAR
  ) where

import Data.Int
import Data.Binary
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import qualified Data.ByteString.Lazy as B
import Data.String.Utils

import Control.Arrow
import Control.Applicative
import Control.Monad

-- import JVM.Dump

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable

import Data.IORef
import System.IO.Unsafe
import System.Directory

import JVM.ClassFile
import JVM.Converter
import Java.ClassPath hiding (Directory)
import Java.JAR

import {-# SOURCE #-} MateVMRuntime.MethodPool
import MateVMRuntime.Types
import MateVMRuntime.Debug
import {-# SOURCE #-} MateVMRuntime.GarbageAlloc
import MateVMRuntime.NativeSizes
import {-# SOURCE #-} MateVMRuntime.ClassHierarchy

getClassInfo :: B.ByteString -> IO ClassInfo
getClassInfo path = do
  class_map <- getClassMap
  case M.lookup path class_map of
    Nothing -> loadAndInitClass path
    Just ci -> return ci

getClassInfoNoInit :: B.ByteString -> IO ClassInfo
getClassInfoNoInit path = do
  class_map <- getClassMap
  case M.lookup path class_map of
    Nothing -> loadClassNoInit path
    Just ci -> return ci

classLoaded :: B.ByteString -> IO Bool
classLoaded path = do
  class_map <- getClassMap
  return $ M.member path class_map

getClassFile :: B.ByteString -> IO (Class Direct)
getClassFile path = do
  ci <- getClassInfo path
  return $ ciFile ci

getStaticFieldOffset :: B.ByteString -> B.ByteString -> IO CPtrdiff
getStaticFieldOffset path field = do
  ci <- getClassInfo path
  return $ fromIntegral $ ciStaticMap ci M.! field

getFieldOffset :: B.ByteString -> B.ByteString -> IO Int32
getFieldOffset path field = do
  ci <- getClassInfo path
  return $ ciFieldMap ci M.! field

-- class name, methodname, methodsignature
getMethodOffset :: B.ByteString -> B.ByteString -> B.ByteString -> IO NativeWord
getMethodOffset path method sig = do
  ci <- getClassInfo path
  -- (+ ptrSize): one slot for "interface-table-ptr"
  return $ (+ ptrSize) $ fromIntegral $
    ciMethodMap ci M.! (method `B.append` sig)

getMethodTableNoInit :: B.ByteString -> IO NativeWord
getMethodTableNoInit path = do
  ci <- getClassInfoNoInit path
  return $ ciMethodBase ci

getMethodTable :: B.ByteString -> IO NativeWord
getMethodTable path = do
  ci <- getClassInfo path
  return $ ciMethodBase ci

getMethodTableReverse :: NativeWord -> IO (Maybe B.ByteString)
getMethodTableReverse mtable = do
  class_map <- getClassMap
  let f x = ciMethodBase x == mtable
  return $ ciName <$> find f (M.elems class_map)

getObjectSize :: B.ByteString -> IO NativeWord
getObjectSize path = do
  fsize <- getFieldCount path
  -- one slot for "method-table-ptr"
  -- one slot for GC-data
  return $ (2 + fsize) * ptrSize

getFieldCount :: B.ByteString -> IO NativeWord
getFieldCount path = do
  ci <- getClassInfo path
  -- TODO(bernhard): correct sizes for different types...
  return $ ciFieldLength ci

-- TODO: not implemented yet. will return empty map!
getStaticFieldTypes :: B.ByteString -> IO [(Int32, FieldSignature)]
getStaticFieldTypes path = do
  ci <- getClassInfo path
  return $ map (second fieldSignature) $ M.toList (ciStaticFieldTypeMap ci)

-- TODO: not very well tested
getFieldTypes :: B.ByteString -> IO [(Int32, FieldSignature)]
getFieldTypes path = do
  ci <- getClassInfo path
  return $ map (second fieldSignature) $ M.toList (ciFieldTypeMap ci)

getFieldSignatures :: FieldTypeMap -> [(Int32, FieldSignature)]
getFieldSignatures m = map (second fieldSignature) $ M.toList m


getStaticFieldAddr :: CPtrdiff -> IO CPtrdiff
getStaticFieldAddr from = do
  trapmap <- getTrapMap
  let w32_from = fromIntegral from
  let sfi = trapmap M.! w32_from
  setTrapMap $ M.delete w32_from trapmap
  case sfi of
    (StaticField (StaticFieldInfo cls field)) -> getStaticFieldOffset cls field
    _ -> error "getFieldAddr: no TrapCause found. abort"

-- interface name, methodname, methodsignature
getInterfaceMethodOffset :: B.ByteString -> B.ByteString -> B.ByteString -> IO NativeWord
getInterfaceMethodOffset ifname meth sig = do
  loadInterface ifname
  ifmmap <- getInterfaceMethodMap
  case M.lookup (ifname `B.append` meth `B.append` sig) ifmmap of
    Just w32 -> return w32
    Nothing -> error "getInterfaceMethodOffset: no offset set"


readClass :: B.ByteString -> IO ClassInfo
readClass path = do
  class_map' <- getClassMap
  case M.lookup path class_map' of
    Just cm -> return cm
    Nothing -> do
      cfile <- readClassFile $ toString path
      -- TODO(bernhard): hDumpClass
      -- dumpClass cfile
      -- load all interfaces, which are implemented by this class
      sequence_ [ loadInterface i | i <- interfaces cfile ]
      superclass <- if path /= "java/lang/Object"
          then do
            sc <- readClass $ superClass cfile
            return $ Just sc
          else return Nothing

      ((staticmap, statictypemap), (fieldmap, fieldtypemap, fsize)) <-
        calculateFields cfile superclass
      (methodmap, mbase, msize) <- calculateMethodMap cfile superclass
      immap <- getInterfaceMethodMap

      -- allocate interface offset table for this class
      -- TODO(bernhard): we have some duplicates in immap (i.e. some
      --                 entries have the same offset), so we could
      --                 save some memory here.
      iftable <- mallocClassData $ ptrSize * M.size immap
      let wn_iftable = fromIntegral $ ptrToIntPtr iftable :: NativeWord
      -- store interface-table at offset 0 in method-table
      pokeElemOff (intPtrToPtr $ fromIntegral mbase) 0 wn_iftable
      let hexDumpMap :: Integral v => String -> M.Map B.ByteString v -> IO ()
          hexDumpMap header mmap = do
            let printValue :: B.ByteString -> IO ()
                printValue key = printfCp $ printf "\t%-70s: 0x%08x\n" (toString key) val
                  where val = fromIntegral (mmap M.! key) :: NativeWord
            printfCp $ printf "%s\n" header
            mapM_ printValue (M.keys mmap)
      when mateDEBUG $ do
        let strpath = toString path
        hexDumpMap ("staticmap @ " ++ strpath) staticmap
        hexDumpMap ("fieldmap @ " ++ strpath) fieldmap
        hexDumpMap ("methodmap @ " ++ strpath) methodmap
        hexDumpMap ("interfacemap @ " ++ strpath) immap
        printfCp $ printf "mbase:   0x%08x\n" mbase
        printfCp $ printf "iftable: 0x%08x\n" wn_iftable
      virtual_map <- getVirtualMap
      setVirtualMap $ M.insert mbase path virtual_map

      class_map <- getClassMap
      let new_ci = ClassInfo
                   { ciName = path
                   , ciFile = cfile
                   , ciStaticMap = staticmap
                   , ciStaticFieldTypeMap = statictypemap
                   , ciFieldMap = fieldmap
                   , ciFieldTypeMap = fieldtypemap
                   , ciFieldLength = fsize
                   , ciMethodMap = methodmap
                   , ciMethodBase = mbase
                   , ciMethodLength = msize
                   , ciInitDone = False
                   }
      setClassMap $ M.insert path new_ci class_map

      -- add Class to Hierarchy
      super_mtable <- case superclass of
        Nothing -> return 0
        Just x -> getMethodTable $ ciName x
      addClassEntry mbase super_mtable (interfaces cfile)

      return new_ci


loadInterface :: B.ByteString -> IO ()
loadInterface path = do
  imap <- getInterfaceMap
  -- interface already loaded?
  case M.lookup path imap of
    Just _ -> return ()
    Nothing -> do
      printfCp $ printf "interface: loading \"%s\"\n" $ toString path
      cfile <- readClassFile $ toString path
      -- load "superinterfaces" first
      sequence_ [ loadInterface i | i <- interfaces cfile ]
      immap <- getInterfaceMethodMap

      -- load map again, because there could be new entries now
      -- due to loading superinterfaces
      imap' <- getInterfaceMap
      let max_off = fromIntegral $ M.size immap * ptrSize
      -- create index of methods by this interface
      let mm = zipbase max_off (classMethods cfile)

      -- create for each method from *every* superinterface an entry too,
      -- but just put in the same offset as it is already in the map
      let (ifnames, methodnames) = unzip $ concat
            [ zip (repeat ifname) (classMethods $ imap' M.! ifname)
            | ifname <- interfaces cfile ]
      let sm = zipWith (\x y -> (entry y, immap M.! getname x y)) ifnames methodnames

      -- merge all offset tables
      setInterfaceMethodMap $ M.fromList sm `M.union` M.fromList mm `M.union` immap
      setInterfaceMap $ M.insert path cfile imap'

      -- add Interface to Hierarchy
      addInterfaceEntry path (interfaces cfile)
  where
    zipbase base = zipWith (\x y -> (entry y, x + base)) [0,ptrSize..]
    entry = getname path
    getname p y = p `B.append` methodName y `B.append` encode (methodSignature y)


calculateFields :: Class Direct -> Maybe ClassInfo
                -> IO ((FieldMap, FieldTypeMap), (FieldMap, FieldTypeMap, NativeWord))
calculateFields cf superclass = do
    -- TODO(bernhard): correct sizes. int only atm

    let (sfields, ifields) = partition (S.member ACC_STATIC . fieldAccessFlags) (classFields cf)

    let sc_sm = getsupermap superclass ciStaticMap

    let sfields_size = fromIntegral $ length sfields
    let statictypemap = zipbasetype (fromIntegral sfields_size) sfields
    staticbase <- mallocStaticData (sfields_size * ptrSize) statictypemap
    let sm = zipbase (fromIntegral $ ptrToIntPtr staticbase) sfields
    -- new fields "overwrite" old ones, if they have the same name
    let staticmap = sm `M.union` sc_sm

    let sc_im = getsupermap superclass ciFieldMap
    let sc_imtype = getsupermap superclass ciFieldTypeMap
    let sc_size :: Num a => a
        sc_size = case superclass of
                    Just x -> fromIntegral $ ciFieldLength x
                    Nothing -> 0
    -- "+ (2*ptrsize)" for the method table pointer and GC data
    let max_off = (+ (2*ptrSize)) $ sc_size * ptrSize
    let im = zipbase max_off ifields
    let imtype = zipbasetype max_off ifields
    -- new fields "overwrite" old ones, if they have the same name
    let fieldmap = im `M.union` sc_im
    let fieldtypemap = imtype `M.union` sc_imtype
    let fsize = sc_size + fromIntegral (M.size im)

    return ((staticmap, statictypemap), (fieldmap, fieldtypemap, fsize))
  where
    zipbase :: Int32 -> [Field Direct] -> FieldMap
    zipbase base = foldr (\(x,y) -> M.insert (fieldName y) (x + base)) M.empty . zip [0,ptrSize..]
    zipbasetype :: Int32 -> [Field Direct] -> FieldTypeMap
    zipbasetype base = foldr(\(x,y) -> M.insert (x + base) y) M.empty . zip [0,ptrSize..]

-- helper
getsupermap :: Maybe ClassInfo -> (ClassInfo -> M.Map k v) -> M.Map k v
getsupermap superclass getter = case superclass of Just x -> getter x; Nothing -> M.empty


calculateMethodMap :: Class Direct -> Maybe ClassInfo -> IO (FieldMap, NativeWord, NativeWord)
calculateMethodMap cf superclass = do
    let methods = filter
                  (\x -> (not . S.member ACC_STATIC . methodAccessFlags) x &&
                         ((/=) "<init>" . methodName) x)
                  (classMethods cf)
    let sc_mm = getsupermap superclass ciMethodMap
    let sc_size :: Num a => a
        sc_size = case superclass of
                    Just x -> fromIntegral $ ciMethodLength x
                    Nothing -> 0
    let max_off = sc_size * ptrSize
    let mm = M.fromList $ zipbase max_off methods
    let methodmap = mm `M.union` sc_mm

    let size = M.size sc_mm + sc_size
    -- (+1): one slot for the interface-table-ptr
    methodbase <- mallocClassData $ (size + 1) * ptrSize
    return ( methodmap
           , fromIntegral $ ptrToIntPtr methodbase
           , fromIntegral $ size)
  where zipbase base = zipWith (\x y -> (entry y, x + base)) [0,ptrSize..]
          where entry y = methodName y `B.append` encode (methodSignature y)


loadClassNoInit :: B.ByteString -> IO ClassInfo
loadClassNoInit path = do
  class_map <- getClassMap
  ci <- case M.lookup path class_map of
    Nothing -> readClass path
    Just x -> return x

  when (path /= "java/lang/Object") (void $ loadClassNoInit $ superClass $ ciFile ci)

  class_map' <- getClassMap
  setClassMap $ M.insert path ci class_map'
  return ci


loadAndInitClass :: B.ByteString -> IO ClassInfo
loadAndInitClass path = do
  class_map <- getClassMap
  ci <- case M.lookup path class_map of
    Nothing -> readClass path
    Just x -> return x

  -- first try to execute class initializer of superclass
  when (path /= "java/lang/Object") (void $ loadAndInitClass $ superClass $ ciFile ci)

  -- execute class initializer
  unless (ciInitDone ci) $ case lookupMethod "<clinit>" (ciFile ci) of
    Just _ -> do
      let mi = MethodInfo "<clinit>" path $ MethodSignature [] ReturnsVoid
      entry <- lookupMethodEntry mi
      -- TODO(bernhard): test exception handling in static initalizer
      printfCp $ printf "executing static initializer from %s now\n" (toString path)
      executeFuncPtr (fromIntegral entry)
      printfCp $ printf "static initializer from %s done\n" (toString path)
    Nothing -> return ()

  class_map' <- getClassMap
  let new_ci = ci { ciInitDone = True }
  setClassMap $ M.insert path new_ci class_map'
  return new_ci


readClassFile :: String -> IO (Class Direct)
readClassFile path' = readIORef classPaths >>= rcf
  where
    path = replace "." "/" path'
    rcf :: [MClassPath] -> IO (Class Direct)
    rcf [] = readIORef classPaths >>= \cp -> error $ "readClassFile: Class \"" ++ show path ++ "\" not found." ++ "\n" ++ show cp
    rcf (Directory pre:xs) = do
      let cf = pre ++ path ++ ".class"
      printfCp $ printf "rcf: searching @ %s for %s\n" (show pre) (show path)
      b <- doesFileExist cf
      if b
        then parseClassFile cf
        else rcf xs
    rcf (JAR p:xs) = do
      printfCp $ printf "rcf: searching %s in JAR\n" (show path)
      entry <- getEntry p path
      case entry of
        Just (LoadedJAR _ cls) -> return cls
        Nothing -> rcf xs
        _ -> error $ "readClassFile: Class \"" ++ show path ++ "\" in JAR not found. #1"

data MClassPath =
  Directory String |
  JAR [Tree CPEntry]
  deriving (Show)

classPaths :: IORef [MClassPath]
{-# NOINLINE classPaths #-}
classPaths = unsafePerformIO $ newIORef []

addClassPath :: String -> IO ()
addClassPath x = do
  cps <- readIORef classPaths
  writeIORef classPaths (Directory x:cps)

addClassPathJAR :: String -> IO ()
addClassPathJAR x = do
  cps <- readIORef classPaths
  t <- execClassPath $ addJAR x
  writeIORef classPaths (JAR t:cps)
