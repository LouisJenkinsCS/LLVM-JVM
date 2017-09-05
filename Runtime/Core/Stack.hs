module Runtime.Core.Stack where
  import Runtime.Core.Frame (Frame)

  import Data.Array.IO


  type Stack = IOUArray Int Frame
