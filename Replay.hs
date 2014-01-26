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
newtype ReplayT m q r a = ReplayT {
  runReplayT :: Trace r -> m ((Either q a, Trace r), Trace r)
  }

instance (Monad m) => Monad (ReplayT m q r) where
  return a = ReplayT $ \t -> return ((Right a, emptyTrace), t)
  (ReplayT f) >>= f' = ReplayT $ \t ->
    do ((x1, w1), t1) <- f t
       case x1 of
         (Left q1) -> return ((Left q1, w1), t1)
         (Right a1) -> do ((x2, w2), t2) <- runReplayT (f' a1) t1
                          return ((x2, w1 ++ w2), t2)

liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
liftR c = ReplayT $ \t -> c >>= \x ->
  return ((Right x, emptyTrace), t)

type Replay q r a = ReplayT IO q r a

-- | lift IO into Replay
io :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
io m = ReplayT $ \tr -> case tr of
  (t@(Result s) : ts) -> return ((Right (read s), [t]), ts)
  ts -> do a <- m
           return ((Right a, [Result (show a)]), ts)

-- | Conditional continuation when paired up with an answer
ask :: (Monad m) => q -> ReplayT m q r r
ask q = ReplayT $ \tr ->
  case tr of
    (t@(Answer r) : ts) -> return ((Right r, [t]), ts)
    ts -> return ((Left q, emptyTrace), ts)

-- | Run replay
run :: (Monad m, Show a, Read a) =>
       ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
run m tr = do
  ((x, w), t) <- runReplayT m tr
  case x of
    (Left q) -> return $ Left (q, w)
    (Right a) -> return $ Right a
