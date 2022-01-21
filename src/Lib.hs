module Lib
    ( printExamples
    ) where

import qualified ASTDecomposed
import qualified AST
import qualified ASTIf
import qualified BinTree
import qualified FreeList
import qualified FreeState

printExamples :: IO ()
printExamples = do
  putStrLn "ASTDecomposed.ex1"
  ASTDecomposed.ex1
  putStrLn "AST.ex1"
  AST.ex1
  putStrLn "AST.ex2"
  AST.ex2
  putStrLn "ASTIf.ex1"
  ASTIf.ex1
  putStrLn "ASTIf.ex2"
  ASTIf.ex2
  putStrLn "ASTIf.ex3"
  ASTIf.ex3
  putStrLn "BinTree.ex1"
  BinTree.ex1
  putStrLn "FreeList.ex1"
  FreeList.ex1
  putStrLn "FreeList.ex2"
  FreeList.ex2
  putStrLn "FreeState.ex1"
  FreeState.ex1
