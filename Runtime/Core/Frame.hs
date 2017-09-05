module Runtime.Core.Frame where
  import Runtime.Core.Types

  import Data.Array.IO
  import Data.IORef

  import Control.Monad.IO.Class



  load :: Integral a => a -> Runtime Variable
  load idx = do
    frame <- currentFrame
    liftIO $ readArray (variables frame) (fromIntegral idx)

  store :: Integral a => Frame -> a -> Variable -> Runtime ()
  store frame idx var = liftIO $ writeArray (variables frame) (fromIntegral idx) var

  modifyVariable :: Integral a => Frame -> a -> (Variable -> Variable) -> Runtime ()
  modifyVariable frame idx f = do
    var <- load frame idx
    let var' = f var in store frame idx var'
