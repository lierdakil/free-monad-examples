{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}

-- | Exercise 02
module Solutions.Store where

import Data.IORef
import Control.Monad.Free

type Key = String
type Value = String

data StoreF a
  = GetValue Key (Maybe Value -> a)
  | PutValue Key Value a
  deriving Functor

type Store = Free StoreF

getValue :: Key -> Store (Maybe Value)
getValue k = liftF $ GetValue k id

putValue :: Key -> Value -> Store ()
putValue k v = liftF $ PutValue k v ()

runStoreF :: IORef [(Key, Value)] -> StoreF a -> IO a
runStoreF ref = \case
  GetValue k next -> next . lookup k <$> readIORef ref
  PutValue k v next -> do
    writeIORef ref . ((k,v):) =<< readIORef ref
    pure next

runStore :: Store a -> IO a
runStore act = do
  ref <- newIORef []
  foldFree (runStoreF ref) act

ex1 :: IO ()
ex1 = print =<< runStore do
  putValue "test" "1"
  putValue "test2" "2"
  putValue "test3" "42"
  getValue "test2"
