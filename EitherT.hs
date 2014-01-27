module EitherT (
  EitherT (..)
  , liftL
  ) where

import Control.Monad
import Control.Monad.Trans

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Monad m) => Monad (EitherT e m) where
  return a = EitherT $ return (Right a)
  m >>= f = EitherT $ do
    x <- runEitherT m
    case x of
      (Left a) -> return (Left a)
      (Right a) -> runEitherT (f a)

instance MonadTrans (EitherT e) where
  lift m = EitherT (Right `liftM` m)

liftL :: (Monad m) => e -> m a -> EitherT e m a
liftL e m = EitherT (return (Left e))
