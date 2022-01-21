{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}

module ASTIf where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Free

data ASTF t a
  = Add t t (t -> a)
  | Input (t -> a)
  | Output t a
  | If t a a
  deriving Functor

type FreeAST t = Free (ASTF t)

input :: FreeAST t t
input = liftF $ Input id

add :: t -> t -> FreeAST t t
add x y = liftF $ Add x y id

output :: t -> FreeAST t ()
output x = liftF $ Output x ()

if' :: t -> FreeAST t a -> FreeAST t a -> FreeAST t a
if' cond t f = wrap $ If cond t f

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
      If cond t f ->
        if cond /= 0 then pure t else pure f

printAST :: FreeAST String a -> String
printAST fast = snd $ evalState (runWriterT $ go fast) 0
  where
    freshVar :: State Int String
    freshVar = do
      n <- get
      put $ n + 1
      pure $ "x" <> show n
    go :: FreeAST String a -> WriterT String (State Int) ()
    go (Pure _) = pure ()
    go (Free f) = case f of
      Add x y g -> do
        var <- lift freshVar
        tell $
          var <> " <- add " <> x <> " " <> y <> "\n"
        go $ g var
      Input x -> do
        var <- lift freshVar
        tell $ var <> " <- input\n"
        go $ x var
      Output s next -> do
        tell $ "output " <> s <> "\n"
        go next
      If cond onTrue onFalse -> do
        tell $ "if' " <> cond <> " (do\n"
        _ <- go onTrue
        tell "\n) (do\n"
        _ <- go onFalse
        tell "\n)\n"

someAST :: (Read a, Show a) => FreeAST a ()
someAST = do
  x <- input
  y <- input
  res <- if' x (add x y) (pure y)
  output res

ex1, ex2, ex3, ex4 :: IO ()
ex1 = computeAST program
ex2 = putStrLn $ printAST program
ex3 = computeAST someAST
ex4 = putStrLn $ printAST someAST
