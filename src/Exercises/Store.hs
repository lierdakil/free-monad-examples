{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Exercise 02
module Exercises.Store where

import Data.IORef
import Control.Monad.Free

type Key = String
type Value = String

-- | Void datatype to mark TODO type definitions
data TODO

data StoreF a = TODO -- TODO
  deriving Functor

type Store a = TODO

getValue :: Key -> Store (Maybe Value)
getValue = undefined -- TODO

putValue :: Key -> Value -> Store ()
putValue = undefined -- TODO

runStoreF :: IORef [(Key, Value)] -> StoreF a -> IO a
runStoreF = undefined -- TODO

runStore :: Store a -> IO a
runStore = undefined -- TODO
