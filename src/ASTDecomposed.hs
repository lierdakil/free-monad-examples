{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}

module ASTDecomposed where

import Control.Monad.Free

data ArithASTF t a = Add t t (t -> a)
  deriving Functor

data IOASTF t a = Input (t -> a) | Output t a
  deriving Functor

data ASTF t a
  = Arith (ArithASTF t a)
  | IO (IOASTF t a)
  deriving Functor

type FreeAST t = Free (ASTF t)

input :: FreeAST t t
input = liftF $ IO $ Input id

add :: t -> t -> FreeAST t t
add x y = liftF $ Arith $ Add x y id

output :: t -> FreeAST t ()
output x = liftF $ IO $ Output x ()

program :: (Read a, Show a) => FreeAST a ()
program = do
  x <- input
  y <- input
  res <- add x y
  output res

computeAST :: FreeAST Int () -> IO ()
computeAST = foldFree computeASTF

computeASTF :: (Num t, Read t, Show t) => ASTF t x -> IO x
computeASTF arg = case arg of
  Arith c -> pure $ computeArith c
  IO c -> computeIO c

computeArith :: Num t => ArithASTF t a -> a
computeArith (Add x y next) = next (x + y)

computeIO :: (Read t, Show t) => IOASTF t a -> IO a
computeIO arg = case arg of
  Input next -> do
    putStr "input> "
    next . read <$> getLine
  Output x next -> do
    print x
    pure next

ex1 :: IO ()
ex1 = computeAST program
