module Runtime.Core.ClassLoader where
  import Parser.Class.ClassFile (ClassFile, parseClassFile)

  {-
    The ClassLoader is responsible for loading all '.class' files, parsing them,
    and managing runtime types. In terms of the JVM, this is considered the 'Bootstrap'
    class loader.
  -}

  data ClassLoader = ClassLoader {
    -- We manage all class file instances here...

  }
