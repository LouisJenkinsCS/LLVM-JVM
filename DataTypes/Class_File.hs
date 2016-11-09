module DataTypes.Class_File where
  import Data.Word
  import Data.ByteString.Char8 as CBS
  import qualified Data.ByteString as BS
  import DataTypes.Constant_Pool
  import DataTypes.Parse_Bytes
  import Control.Monad.State.Lazy
  import DataTypes.Attributes
  import DataTypes.Methods
  import DataTypes.Fields
  import VirtualMachine.Stack
  import DataTypes.Methods

  {-
    Data structure representation of a `.class` file.
  -}
  data ClassFile = ClassFile {
      cp_info :: [CP_Info],
      access_flags :: Word16,
      this_class :: Word16,
      super_class :: Word16,
      interfaces :: [Word16],
      fields :: [Field_Info],
      methods :: [Method_Info],
      classfile_attributes :: [Attribute_Info]
  } deriving (Show)

  {-
    Parameter arguments are specified as such: (x -> y -> z), wherein it will take 'x' and 'y'
    and return 'z'. Some arguments however are implicit, such as monads, which implicitly
    take and return a state.

    'parseClassFile' takes a 'StateT' monad, which implicitly passes the state of a
    'ByteString' with each invocation. By doing so it allows it to simulate side-effects
    while remaining functionally pure, as the results of the previous computation carries
    on to the next without causing an actual side-effect.

    The 'StateT' monad can use 'evalState' to immediately evaluate all computations and return
    the result (Remember, Haskell is very lazy, and computations are not evaluated until needed,
    this would force such evaluations).

    Monads generally use binding, either with the '>>=' infix bind operator, which
    obtains the result of the left-hand side computation and passes it to the right-hand side.
    The '>>' operator will discard the result but complete the computation, which is handy for
    discarding irrelevant data.

    The <$> operator is the 'fmap' infix operator, which applies the functor ('StateT' is a monadic functor)
    to the result of applying a mapping of an input to an output. I.E: (+1) <$> Just 2 => Just 3

    The <*> operator is the 'sequence' inifx operator, which is similar to bind, except it is left-associative.
    It works with the below due to currying: Currying is the process of taking a function, say (x -> y -> z),
    supplying a partial application, resulting in (y -> z) where 'x' has been given a value. Hence the next sequential
    operation can supply the remaining argument 'y'. Example...

    (x -> y -> z) <$> getX => (y -> z),
    (x -> y -> z) <$> getX <*> getY => z.

    The 'replicateM' performs the passed action 'n' times and combines the result into a list.

    Syntax such as: (\x -> x + 1) constitute a lambda function.
  -}
  parseClassFile :: ByteString -> ClassFile
  parseClassFile = evalState $ getNextInt >> getNextShort >> getNextShort -- Discard magic and version information
    >> parseConstants >>= \cp -> ClassFile cp <$> getNextShort <*> getNextShort <*> getNextShort
    <*> parseInterfaces <*> parseFields cp <*> parseMethods cp <*> parseAttributes cp
      where
        parseInterfaces = getNextShort >>= \n -> replicateM (fromIntegral n) getNextShort

  getMain :: [CP_Info] -> [Method_Info] -> Method_Info
  getMain cp (x:xs)
    | show (utf8_bytes (cp !! fromIntegral (method_name_index x))) == "main" = x
    | otherwise = getMain cp xs
  getMain _ [] = error "Main not found"



  main :: IO ()
  main = do
    r0 <- BS.readFile "AdditionTest.class"
    Prelude.putStrLn $ "Starting Bytes: " ++ show (BS.length r0)
    let classFile = parseClassFile r0
    Prelude.putStrLn $ show classFile
    stack <- bootstrap
    str <- debugStack stack
    Prelude.putStrLn $ "Pre-Stack: " ++ str
    Prelude.putStrLn $ "Setup Stack..."
    let m = getMain (cp_info classFile) (methods classFile)
    let codeAttr = getCodeAttribute (cp_info classFile) (method_attributes m)
    pushFrame stack (max_locals codeAttr) (code codeAttr)
    str' <- debugStack stack
    Prelude.putStrLn $ "Post-Stack: " ++ str'
    debugExec stack
    str'' <- debugStack stack
    Prelude.putStrLn $ "Final-Stack: " ++ str''
