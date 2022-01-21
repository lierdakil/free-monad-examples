{-# LANGUAGE DeriveFunctor #-}

-- | Exercise 01
module Solutions.ReaderWriter where

import Data.Tuple (swap)
import Control.Monad.Free

newtype ReaderF r a = ReaderF { runReaderF :: r -> a }
  deriving Functor

type Reader r = Free (ReaderF r)

ask :: Reader r r
ask = liftF $ ReaderF id

runReader :: Reader r a -> r -> a
runReader = foldFree runReaderF

newtype WriterF w a = WriterF { runWriterF :: (w, a) }
  deriving Functor

type Writer w = Free (WriterF w)

tell :: w -> Writer w ()
tell w = liftF $ WriterF (w, ())

listen :: Monoid w => Writer w a -> Writer w (a, w)
listen act =
  let ~(a, w) = runWriter act
  in liftF $ WriterF (w, (a, w))

pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
pass act =
  let ~((a, f), w) = runWriter act
  in liftF $ WriterF (f w, a)

runWriter :: Monoid w => Writer w a -> (a, w)
runWriter = swap . foldFree runWriterF
