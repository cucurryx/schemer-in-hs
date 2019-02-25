module Parser where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           System.Environment
import           Control.Monad
import           Data.Ratio
import           Data.Array
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
             | Vector (Array Int LispVal)       -- #(1 2 3)


instance Show LispVal where show = showVal

-- serialization function for LispVal
showVal :: LispVal -> String
showVal (Atom atom) = atom
showVal (String str) = "\"" ++ str ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character char) = [char]
showVal (Number num) = show num
showVal (Float float) = show float
showVal (Ratio ratio) = show ratio
showVal (Vector vec) = "(vector" ++ show vec ++ ")"
showVal (List list) = "(" ++ serializeList list ++ ")"
showVal (DottedList front last) = "(" ++ serializeList front ++ " . " ++ showVal last ++ ")"

serializeList :: [LispVal] -> String 
serializeList = unwords . map showVal

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- read a expr, and handle all possible errors
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "read lisp expr" input of
    Left  err -> String $ "Error: " ++ show err
    Right val -> val

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
parseList = List <$> sepBy parseExpr spaces

-- parser for DottedList
-- (1 2 3 . 4) -> DootedList [1, 2, 3] 4
parseDottedList :: Parser LispVal
parseDottedList = do
    first <- endBy parseExpr spaces
    rest <- char '.' >> spaces >> parseExpr
    return $ DottedList first rest
 
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
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quoted", x]

-- parser for unquoted
parseUnquoted :: Parser LispVal
parseUnquoted = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquoted", x]

-- parser for quasiquoted
parseQQuoted :: Parser LispVal
parseQQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quosiquoted", x]

-- parser for list or dotted list
parseList' :: Parser LispVal
parseList' = do 
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

-- parser for vector
parseVector :: Parser LispVal
parseVector = do
    values <- sepBy parseExpr spaces
    return $ Vector (listArray (0, length values - 1) values)

-- wrapper for parser of vector
parseVector' :: Parser LispVal
parseVector' = do
    string "#("
    x <- parseVector
    string ")"
    return x

-- parse lisp expression
parseExpr :: Parser LispVal
parseExpr =
    parseAtom
        <|> parseString
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseFloat
        <|> try parseQuoted
        <|> try parseVector'
        <|> parseList'
        <|> parseUnquoted
        <|> parseQQuoted