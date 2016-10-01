module Main where

import qualified Data.Map as M
import Test.QuickCheck
import Control.Applicative
import Control.Monad

import CNF

main :: IO ()
main = do
    Formula values expr <- generate (sizedFormula 4)
    printValues values
    printExpr expr
    _ <- getLine
    printExpr $ toCnf expr
    _ <- getLine
    print $ eval (evaluate expr) values
    _ <- getLine
    main
    where
        printValues = flip forM_ (\(k, v) -> putStrLn $ k : " = " ++ show v) . M.toList
        printExpr = putStrLn . prettyPrint

