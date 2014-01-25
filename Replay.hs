module Replay (
  -- * Replay
  Replay
  , io
  , ask
  , run
  , running

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
newtype Replay q r a = R { runReplay :: Trace r -> IO ((Either q a, Trace r), Trace r) }

instance Monad (Replay q r) where
  return a = R $ \t -> return ((Right a, emptyTrace), t)
  m >>= f = R $ \t -> do
    ((x1, w1), t1) <- runReplay m t
    case x1 of
      (Left q1) -> return ((Left q1, w1), t1)
      (Right a1) -> do ((x2, w2), t2) <- runReplay (f a1) t1
                       return ((x2, w1 ++ w2), t2)

-- | lift IO into Replay
io :: (Show a, Read a) => IO a -> Replay q r a
io m = R $ \tr -> case tr of
  (t@(Result s) : ts) -> return ((Right (read s), [t]), ts)
  ts -> do a <- m
           return ((Right a, [Result (show a)]), ts)

-- | Conditional continuation when paired up with an answer
ask :: q -> Replay q r r
ask q = R $ \tr ->
  case tr of
    (t@(Answer r) : ts) -> return ((Right r, [t]), ts)
    ts -> return ((Left q, emptyTrace), ts)

-- | Run replay
run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run m tr = do
  ((x, w), t) <- runReplay m tr
  case x of
    (Left q) -> return $ Left (q, w)
    (Right a) -> return $ Right a

-- | Iterative Q&A
running :: Replay String String a -> IO a
running prog = play emptyTrace
  where play t = do
          rlt <- run prog t
          case rlt of
            (Left (q, t')) -> do
              putStrLn $ "Question: " ++ q ++ " "
              r <- getLine
              play (addAnswer t' r)
            (Right a) -> return a
