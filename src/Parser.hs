module Parser where 

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

-- lisp ADT
data ListVal = Atom String                      -- atom
             | List [ListVal]                   -- (x y z)
             | DottedList [ListVal] ListVal     -- (x y . z)
             | Number Integer                   -- 1
             | String String                    -- "this is string"
             | Bool Bool                        -- #t / #f
             | Character Char                   -- 'a'

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- read a expr, and handle all possible errors
readExpr :: String -> String
readExpr input = case parse parseExpr "read lisp expr" input of
    Left err -> "Error: " ++ show err
    Right val -> "Found value"

-- parser for Atom
-- An atom is a letter or symbol, followed by any number of letters, digits, or symbols
parseAtom :: Parser ListVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    let x = first:rest
    return $ case x of 
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom x

-- parser for List
parseList :: Parser ListVal
parseList = return $ String "TODO"

-- parser for DottedList
parseDottedList :: Parser ListVal
parseDottedList = return $ String "TODO"

-- parser for binary number
parseBinary :: Parser ListVal
parseBinary = return $ Number 1

-- parser for octal number
parseOct :: Parser ListVal
parseOct = return $ Number 1

-- parser for decimal number
parseDec :: Parser ListVal
parseDec = return $ Number 1

-- parser for hex number
parseHex :: Parser ListVal
parseHex = return $ Number 1

-- parser for Number
parseNumber :: Parser ListVal
parseNumber = parseBinary <|> parseOct <|> parseDec <|> parseHex

-- parse number without do anotation
-- #b: binary, #o: octal, #h: hexadecimal, #d: decimal 
parseNumber' :: Parser ListVal
parseNumber' = many1 digit >>= return . Number . read

-- parse escape characters like: '\t', '\n', '\"'
-- eat a '\', and then eat a '"' or '\'
parseEscapeChar :: Parser Char
parseEscapeChar = do 
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of 
        '\\' -> x
        '\"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'

-- parser for String
parseString :: Parser ListVal
parseString = do
    char '"'
    x <- many (parseEscapeChar <|> noneOf "\\\"")
    char '"'
    return $ String x

-- parser for quoted
parseQuoted :: Parser ListVal
parseQuoted = return $ String "TODO"

-- parse lisp expression
parseExpr :: Parser ListVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        -- <|> parseList
        -- <|> parseDottedList