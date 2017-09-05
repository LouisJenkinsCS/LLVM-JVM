module Runtime.Core.Frame where
  import Runtime.Core.Types
  import Data.IORef
  import Data.Array.IO
  import Control.Monad.IO.Class

  load :: Integral a => a -> Runtime Variable
  load idx = do
    frame <- currentFrame
    liftIO $ readArray (variables frame) (fromIntegral idx)

  store :: Integral a => a -> Variable -> Runtime ()
  store idx var = do
    frame <- currentFrame
    liftIO $ writeArray (variables frame) (fromIntegral idx) var

  modify :: Integral a => a -> (Variable -> Variable) -> Runtime ()
  modify idx f = do
    var <- load idx
    store idx (f var)

  push :: Variable -> Runtime ()
  push var = do
    frame <- currentFrame
    liftIO $ do
      idx <- atomicModifyIORef' (operandTop frame) (\x -> (x + 1, x))
      writeArray (operands frame) idx var

  pop :: Runtime Variable
  pop = do
    frame <- currentFrame
    liftIO $ do
      idx <- atomicModifyIORef' (operandTop frame) (\x -> (x - 1, x - 1))
      readArray (operands frame) idx
