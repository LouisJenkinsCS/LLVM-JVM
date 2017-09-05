module Runtime.Core.Environment where
  import Runtime.Core.Frame
  import Runtime.Core.Stack

  import Control.Monad.Trans.Class
  import Control.Monad
  import Control.Monad.Reader

  {-
    The Runtime Environment for the JVM, which encompasses all state.
  -}

  {-
    Types
  -}

  data Environment = Environment {
    -- TODO
  }

  -- The runtime state.
  type Runtime = ReaderT Environment IO

  -- Convenience typeclass to allow easy access...
  class (Monad m) => RuntimeEnvironment m where
    currentFrame :: m Frame
    stackFrame :: m Stack

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
