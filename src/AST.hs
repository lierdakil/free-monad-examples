{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}

module AST where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Free

data ASTF t a
  = Add t t (t -> a)
  | Input (t -> a)
  | Output t a
  deriving Functor

type FreeAST t = Free (ASTF t)

input :: FreeAST t t
input = liftF $ Input id

add :: t -> t -> FreeAST t t
add x y = liftF $ Add x y id

output :: t -> FreeAST t ()
output x = liftF $ Output x ()

program :: (Read a, Show a) => FreeAST a ()
program = do
  x <- input
  y <- input
  res <- add x y
  output res

computeAST :: FreeAST Int () -> IO ()
computeAST = foldFree go
  where
    go :: ASTF Int x -> IO x
    go arg = case arg of
      Add x y next -> pure $ next (x + y)
      Input next -> do
        putStr "input> "
        next . read <$> getLine
      Output x next -> do
        print x
        pure next

printAST :: FreeAST String () -> String
printAST fast = snd $ evalState (runWriterT $ foldFree go fast) 0
  where
    freshVar :: State Int String
    freshVar = do
      n <- get
      put $ n + 1
      pure $ "x" <> show n
    go :: ASTF String a -> WriterT String (State Int) a
    go f = case f of
      Add x y g -> do
        var <- lift freshVar
        tell $
          var <> " <- add " <> x <> " " <> y <> "\n"
        pure $ g var
      Input x -> do
        var <- lift freshVar
        tell $ var <> " <- input\n"
        pure $ x var
      Output s next -> do
        tell $ "output " <> s <> "\n"
        pure next

ex1, ex2 :: IO ()
ex1 = computeAST program
ex2 = putStrLn $ printAST program
