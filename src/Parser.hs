module Parser where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           System.Environment
import           Control.Monad
import           Data.Ratio
import           Numeric

-- lisp ADT
data LispVal = Atom String                      -- atom
             | List [LispVal]                   -- (x y z)
             | DottedList [LispVal] LispVal     -- (x y . z)
             | Number Integer                   -- 1
             | String String                    -- "this is string"
             | Bool Bool                        -- #t / #f
             | Character Char                   -- 'a'
             | Float Double                     -- 1.22
             | Ratio Rational                   -- 1/2  

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- read a expr, and handle all possible errors
readExpr :: String -> String
readExpr input = case parse parseExpr "read lisp expr" input of
    Left  err -> "Error: " ++ show err
    Right val -> "Found value"

-- parser for Bool
parseBool :: Parser LispVal
parseBool =
    do
            char '#'
            char 't' >> return (Bool True)
        <|> (char 'f' >> return (Bool False))


-- parser for Atom
-- An atom is a letter or symbol, followed by any number of letters, digits, or symbols
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> symbol <|> digit)
    let x = first : rest
    return $ case x of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom x

-- parser for List
parseList :: Parser LispVal
parseList = return $ String "TODO"

-- parser for DottedList
parseDottedList :: Parser LispVal
parseDottedList = return $ String "TODO"

-- convert binary string to integer
binaryToInt :: String -> Integer
binaryToInt = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

-- parser for binary number
parseBinary :: Parser LispVal
parseBinary = do
    try $ string "#b"
    x <- many1 $ oneOf "01"
    return $ Number $ binaryToInt x

-- parser for octal number
parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- many1 octDigit
    return $ Number $ fst $ head $ readOct x

-- parser for decimal number
parseDec :: Parser LispVal
parseDec = do
    try $ string "#d"
    Number . read <$> many1 digit

-- parser for hex number
parseHex :: Parser LispVal
parseHex = do
    -- consume nothing if error happens
    try $ string "#h"
    x <- many1 hexDigit
    return $ Number $ fst $ head $ readHex x

-- parse number without do anotation
-- #b: binary, #o: octal, #h: hexadecimal, #d: decimal 
parseNumber' :: Parser LispVal
parseNumber' = many1 digit >>= return . Number . read

-- parser for Number
parseNumber :: Parser LispVal
parseNumber =
    parseNumber' <|> parseBinary <|> parseOct <|> parseDec <|> parseHex

-- parse escape characters like: '\t', '\n', '\"'
-- eat a '\', and then eat a '"' or '\'
parseEscapeChar :: Parser Char
parseEscapeChar = do
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
        '\\' -> x
        '\"' -> x
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'

-- parser for String
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (parseEscapeChar <|> noneOf "\\\"")
    char '"'
    return $ String x

-- parser for Character
-- #\space -> ' '
-- #\newline -> '\n'
-- #\a -> 'a'
parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    val <- try (string "space") <|> try (string "newline") <|> singleChar
    return $ Character $ case val of
        "space"   -> ' '
        "newline" -> '\n'
        _         -> head val
  where
    singleChar = do
        x <- anyChar
        notFollowedBy alphaNum
        return [x]

-- parser for Float
parseFloat :: Parser LispVal
parseFloat = do
    int <- many1 digit
    char '.'
    dec <- many1 digit
    return $ Float $ fst $ head $ readFloat (int ++ "." ++ dec)

-- parser for Ratio
-- Ratio of Haskell is constructed by %
parseRatio :: Parser LispVal
parseRatio = do 
    numerator <- many1 digit
    char '/'
    denominator <- many1 digit
    return $ Ratio (read numerator % read denominator)

-- parser for quoted
parseQuoted :: Parser LispVal
parseQuoted = return $ String "TODO"

-- parse lisp expression
parseExpr :: Parser LispVal
parseExpr =
    parseAtom
        <|> parseString
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseFloat
        -- <|> parseList
        -- <|> parseDottedList
