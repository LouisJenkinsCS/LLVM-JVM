module VirtualMachine.Class where
  import ClassFile.Types
  import VirtualMachine.Types
  import GHC.IORef
  import qualified Data.Map as Map

  toClass :: ClassFile -> IO Class
  toClass cf = Class <$> return (cp_info cf) <*> newIORef extractMethods <*> newIORef Map.empty
    where
      extractMethods :: Map.Map String Method
      extractMethods = Map.fromList [extractMethod method_info | method_info <- methods cf]
        where
          extractMethod :: Method_Info -> (String, Method)
          extractMethod info =
            let name = show . utf8_bytes $ cp_info cf !! (fromIntegral . method_name_index $ info)
                code_info = extractCode (method_attributes info)
                method = Method (code code_info) (max_locals code_info)
              in (name, method)
                where
                  extractCode :: [Attribute_Info] -> Attribute_Info
                  extractCode (attr:attrs) = let
                    nindex = attribute_name_index attr
                    name = show . utf8_bytes $ cp_info cf !! fromIntegral nindex
                    in
                      if name == "Code" then attr else extractCode attrs
                  extractCode [] = error "Code Attribute was not found for a method!"
