module Runtime.Core.Environment where
  import Control.Monad.Trans.Class
  import Control.Monad

  {-
    The Runtime Environment for the JVM, which encompasses all state.
  -}

  -- Below is an example using a MonadTransformer. The below will be changed into
  -- and full-blown Environment soon; it is planned to wrap the IO monad at least.


  newtype MaybeT m a = MaybeT {
    runMaybeT :: m (Maybe a)
  }

  returnMT :: (Monad m) => a -> MaybeT m a
  returnMT a = MaybeT $ return (Just a)

  failMT :: (Monad m) => t -> MaybeT m a
  failMT _ = MaybeT $ return Nothing

  bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  x `bindMT` f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

  instance (Monad m) => Functor (MaybeT m) where
    fmap f x = MaybeT $ fmap f <$> runMaybeT x

  instance (Monad m) => Applicative (MaybeT m) where
    pure = returnMT
    (<*>) = ap

  instance (Monad m) => Monad (MaybeT m) where
    return = returnMT
    fail = failMT
    (>>=) = bindMT

  instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)

  fn :: MaybeT IO Int
  fn = return 1

  main :: IO ()
  main = do
    x <- runMaybeT fn
    print x
