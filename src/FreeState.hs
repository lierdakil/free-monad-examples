{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}

module FreeState where

import Control.Monad.Free

newtype StateF s a = StateF { runStateF :: s -> (a, s) }
  deriving Functor

getF :: StateF s s
getF = StateF $ \s -> (s, s)

putF :: s -> StateF s ()
putF s = StateF $ const ((), s)

type State s = Free (StateF s)

liftF' :: Functor f => f a -> Free f a
liftF' = Free . fmap Pure

get :: State s s
get = liftF' getF

put :: s -> State s ()
put = liftF' . putF

someComputation :: State Int ()
someComputation = do
  i <- get
  put $ i + 1
  pure ()

runState :: State s a -> s -> (a, s)
runState (Pure x) s = (x, s)
runState (Free f) s =
  let (m, s') = runStateF f s
  in runState m s'

printState :: (Show s, Show a) => State s a -> s -> String
printState (Pure x) s = "pure (" <> show x <> "," <> show s <> ")"
printState (Free m) s =
  let (x, s') = runStateF m s
  in "state change " <> show s <> " -> " <> show s' <> "\n"
    <> printState x s'

ex1 :: IO ()
ex1 = putStrLn $ printState someComputation 1
