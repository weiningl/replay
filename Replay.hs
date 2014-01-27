module Replay (
  -- * Replay
  ReplayT (..)
  , Replay
  , io
  , ask
  , run

  -- * Trace
  , Trace
  , Item (..)
  , emptyTrace
  , addAnswer
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import EitherT

-- | Trace
type Trace r = [Item r]

-- | Trace Element
data Item r = Answer r | Result String deriving (Show, Read)

-- | Create empty trace
emptyTrace :: Trace r
emptyTrace = []

-- | Add an answer to trace
addAnswer :: Trace r -> r -> Trace r
addAnswer tr r = tr ++ [Answer r]

-- | Replay monad
type ReplayT m q r a = StateT (Trace r) (EitherT q (WriterT (Trace r) m)) a

liftR :: (Monad m) => m a -> ReplayT m q r a
liftR = lift . lift . lift

type Replay q r a = ReplayT IO q r a

-- | lift IO into Replay
io :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
io m = do tr <- get
          case tr of
            ((Result s):ts) -> do put ts
                                  lift. lift. tell $ [Result s]
                                  liftR. return $ read s
            _ -> lift. EitherT. WriterT $ do a <- m
                                             return (Right a, [Result (show a)])

-- | Conditional continuation when paired up with an answer
ask :: (Monad m) => q -> ReplayT m q r r
ask q = do tr <- get
           case tr of
             ((Answer r):ts) -> do put ts
                                   (lift. lift. tell) [Answer r]
                                   (liftR. return) r
             _ -> lift $ EitherT (return (Left q))

-- | Run replay
run :: (Monad m, Show a, Read a) =>
       ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
run m tr = do
  (x, w) <- runWriterT $ runEitherT $ evalStateT m tr
  case x of
    (Left q) -> return $ Left (q, w)
    (Right a) -> return $ Right a

-- run m tr = do
--   (x, w) <- runEitherT $ runWriterT $ evalStateT m tr
--   case x of
--     (Left q) -> return $ Left (q, w)
--     (Right a) -> return $ (Right a)
