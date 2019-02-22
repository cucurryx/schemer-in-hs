module Main where

import Lib 
import Parser
import System.Environment


main :: IO ()
main = do 
    -- get the first arg
    (expr:_) <- getArgs
    putStrLn expr
    putStrLn (readExpr expr)