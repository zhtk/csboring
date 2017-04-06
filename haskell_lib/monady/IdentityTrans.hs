module IdentityTrans where
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Trans

newtype IdentityT m a = IdentityT { runIdentityT :: m a}

instance (Functor m) => Functor (IdentityT m) where
  fmap f = IdentityT . (fmap f) . runIdentityT

instance (Monad m) => Monad (IdentityT m) where
  return = IdentityT . return
  x >>= f = IdentityT $ runIdentityT x >>= \a -> runIdentityT (f a)

instance MonadTrans IdentityT where
  lift = IdentityT

instance MonadPlus m => MonadPlus (IdentityT m) where
  mzero = lift mzero
  mplus m1 m2 = lift $ runIdentityT m1 `mplus` runIdentityT m2

