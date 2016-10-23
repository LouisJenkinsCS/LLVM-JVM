{-
  Method_Area will be used to keep track of the .text segment, or the runtime's
  method area, specified in the JVM. Essentially, when one needs a constant from
  the constant pool, a method to call, field to access, or anything that pertains
  to the constant pool, it will be handled here.

  Obtaining Constants:
    1. Obtain ClassFile representation from class name
    2. Obtain the appropriate constant value
  Invoking Methods:
    1. Obtain the ClassFile representation from class name
    2. Obtain the Methodref_Info representation from the passed descriptor
    3. Find the 'Code' Attribute_Info representation from the method
-}
module VirtualMachine.Method_Area where
  import DataTypes.Class_File
  import qualified Data.Map.Strict as Map
  import qualified VirtualMachine.Stack_Frame

  data Method_Area = MArea {
    constant_pool :: Map.Map String ClassFile
  }

  {-
    Obtain the associated ClassFile with a given class name
  -}
  getClassFile :: String -> Method_Area -> ClassFile

  {-
    Construct a proper stack frame for the requested method of the requested
    class name.
  -}
  invokeMethod :: String -> Word16 -> Method_Area -> Stack_Frame

  {-
    Obtain the constant value requested.
  -}
  obtainedConstantValue :: String -> Word16 -> Method_Area -> Value
