module Main where

import Lib 
import Parser
import Evaluator
import System.Environment


main :: IO ()
main = getArgs >>= print . eval . readExpr . head