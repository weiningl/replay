module Main where

import Data.Time
import Replay

example :: Replay String String Int
example = do
  t0 <- io getCurrentTime
  io $ putStrLn "Hello!"
  age <- ask "What is your age?"
  io $ putStrLn ("You are " ++ age)
  name <- ask "What is your name?"
  io $ putStrLn (name ++ " is " ++ age ++ " years old")
  t1 <- io getCurrentTime
  io $ putStrLn ("Total time: " ++ show (diffUTCTime t1 t0))
  return $ read age

-- | Iterative Q&A
running :: (Show a, Read a) => Replay String String a -> IO a
running prog = play emptyTrace
  where play t = do
          rlt <- run prog t
          case rlt of
            (Left (q, t')) -> do
              putStrLn $ "Question: " ++ q ++ " "
              r <- getLine
              play (addAnswer t' r)
            (Right a) -> return a

main = running example
