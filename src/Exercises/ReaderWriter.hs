{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- Once there's no more undefined, this shouldn't be required
{-# LANGUAGE DeriveFunctor #-}

-- | Exercise 01
module Exercises.ReaderWriter where

import Control.Monad.Free

newtype ReaderF r a = ReaderF { runReaderF :: r -> a }
  deriving Functor

-- | Void datatype to mark TODO type definitions
data TODO

type Reader r a = TODO

ask :: Reader r r
ask = undefined -- TODO

runReader :: Reader r a -> r -> a
runReader = undefined -- TODO

newtype WriterF w a = WriterF { runWriterF :: (w, a) }
  deriving Functor

type Writer w a = TODO

tell :: w -> Writer w ()
tell = undefined -- TODO

listen :: Monoid w => Writer w a -> Writer w (a, w)
listen = undefined -- TODO

pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
pass = undefined -- TODO

runWriter :: Monoid w => Writer w a -> (a, w)
runWriter = undefined -- TODO
