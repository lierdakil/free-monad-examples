{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}

module BinTree where

import Control.Monad.Free
import Text.PrettyPrint.Annotated.WL

data BinTreeF l a = NodeF l a a
  deriving Functor

type FreeBinTree l = Free (BinTreeF l)

buildBalanced :: [Int] -> FreeBinTree Int (Maybe Int)
buildBalanced [] = pure Nothing
buildBalanced [x] = pure $ Just x
buildBalanced xs = do
  let len = length xs
      (l,x:r) = splitAt (len `div` 2) xs
  b <- liftF $ NodeF x l r
  buildBalanced b

data BinTree l = Nil | Leaf l | Branch l (BinTree l) (BinTree l)
  deriving Show

-- | This is just a pretty-printer for 'BinTree'
instance Pretty a => Pretty (BinTree a) where
  pretty = go id
    where
      go _ Nil = text "Nil"
      go _ (Leaf x) = pretty x
      go pars (Branch x l r) = pars $ pretty x <+>
        align (go parens l <#> go parens r)

convert :: FreeBinTree a (Maybe a) -> BinTree a
convert (Pure Nothing) = Nil
convert (Pure (Just x)) = Leaf x
convert (Free f) =
  let NodeF x l r = convert <$> f
  in Branch x l r

ex1 :: IO ()
ex1 = putDoc . pretty $ convert $ buildBalanced [0..10]
