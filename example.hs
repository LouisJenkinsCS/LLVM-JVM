{-
  Syntax examples...
-}

-- Function Syntax

{-
  Functions take parameters using arrows, which may be more familiar in the mathematical
  form of 'f: x -> y', or 'f(x) = y', wherein a function 'f' takes 'x' as an input, and
  returns 'y' as an output.

  A mathematical function of the form 'g: x -> y -> z' may look strange, but may make more
  sense if one looks at it in the form of currying. In summary, currying is the process
  of applying a value to the first argument of a function taking more than one argument,
  resulting in a function that takes the remaining arguments.

  I.E:

  'f(x,y) = z' => 'f(x) = y -> z' => 'f = x -> y -> z'
-}
intAdder :: Int -> Int -> Int
intAdder x y = x + y

{-
  The below is valid syntax. As demonstrated before, a function 'f(x,y) = z'
  can become a partial function 'f(x) = y -> z', which is precisely what it returns,
  a partial application of a function. Note as well that Haskell can automatically pass
  arguments to the right-hand side of the definition of a function. In fact, 'intAdder'
  could be defined as 'intAdder = (+)' as it would pass the first and second argument to
  the 'plus' infix operator.
-}
curriedIntAdder :: Int -> (Int -> Int)
curriedIntAdder = intAdder

{-
  Functions
-}
genericAdder :: (Num a) => a -> a -> a
genericAdder x y = x + y

main :: IO ()
main = return ()
