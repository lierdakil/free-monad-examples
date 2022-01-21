{-# OPTIONS_GHC -Wall #-}
module FreeList where

import Data.List (intercalate)
import Control.Monad.Free

listComputation :: Free [] Int
listComputation = do
  x <- liftF [1, 2, 3]
  y <- liftF [10, 20]
  z <- liftF [100, 200]
  pure $ x+y+z

printFreeList :: Show a => Free [] a -> String
printFreeList (Pure x) = show x
printFreeList (Free f) = "["
  <> intercalate "," (printFreeList <$> f)
  <> "]"

ex1 :: IO ()
ex1 = putStrLn $ printFreeList listComputation

ex2 :: IO ()
ex2 = print $ foldFree id listComputation
