{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}

-- | Exercise 03
module Solutions.BinTreeFold where

import Control.Monad.Free
import Control.Monad
import Text.PrettyPrint.Annotated.WL
import BinTree (BinTreeF(..), FreeBinTree, buildBalanced)

-- | Unlike 'BinTree.BinTree', this type is a 'Monad'
data BinTree l a = Leaf a | Branch l (BinTree l a) (BinTree l a)
  deriving (Show, Functor)

instance Applicative (BinTree l) where
  pure = Leaf
  (<*>) = ap

instance Monad (BinTree l) where
  m >>= f = case m of
    Leaf x -> f x
    Branch x l r -> Branch x (l >>= f) (r >>= f)

-- | This is just a pretty-printer for 'BinTree'
instance (Pretty l, Pretty a) => Pretty (BinTree l a) where
  pretty = go id
    where
      go _ (Leaf x) = pretty x
      go pars (Branch x l r) = pars $ pretty x <+>
        align (go parens l <#> go parens r)

convert :: FreeBinTree l a -> BinTree l a
convert = foldFree \case
  NodeF x l r -> Branch x (Leaf l) (Leaf r)

ex1 :: IO ()
ex1 = putDoc . pretty $ convert $ buildBalanced [0..10]
