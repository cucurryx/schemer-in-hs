module Parser where 

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

-- lisp ADT
data ListVal = Atom String                      -- atom
             | List [ListVal]                   -- (x y z)
             | DottedList [ListVal] ListVal     -- (x y . z)
             | Number Integer                   -- 1
             | String String                    -- "this is string"
             | Bool Bool                        -- #t / #f

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- read a expr, and handle all possible errors
readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "read lisp expr" input of
    Left err -> "Error: " ++ show err
    Right val -> "Found value"

-- parser for Atom
parseAtom :: Parser ListVal
parseAtom = 
    return $ Atom "hello"

-- parser for List
parseList :: Parser ListVal
parseList = return $ String "TODO"

-- parser for DottedList
parseDottedList :: Parser ListVal
parseDottedList = return $ String "TODO"

-- parser for Number
parseNumber :: Parser ListVal
parseNumber = return $ String "TODO"

-- parser for String
parseString :: Parser ListVal
parseString = return $ String "TODO"

-- parser for Bool
parseBool :: Parser ListVal
parseBool = return $ String "TODO"
