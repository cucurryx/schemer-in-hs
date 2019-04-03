module Evaluator where

import           Control.Monad.Except
import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )

import           Parser

-- error ADT
data LispError = NumArgs Integer [LispVal]
                | TryMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String

showError :: LispError -> String
showError _ = "not ok"

instance Show LispError where show = showError

-- the left of Either is LispError, the right is to be given. 
-- for example, `ThrowsError LispVal` means `Either LispError LispVal`
type ThrowsError = Either LispError

-- evaluator 
eval :: LispVal -> LispVal
eval val@(String    _                   ) = val
eval val@(Number    _                   ) = val
eval val@(Bool      _                   ) = val
eval val@(Character _                   ) = val
eval val@(Float     _                   ) = val
eval val@(Ratio     _                   ) = val
eval (    List      [Atom "quoted", val]) = val
eval (    List      (Atom func : args)  ) = call func $ map eval args

-- call function(the first argument) with args(the second argument)
-- if can't get a function from primitives, return Bool False
-- otherwise, apply this function to args
call :: String -> [LispVal] -> LispVal
call func args = maybe (Bool False) (call' args) $ lookup func primitives
    where call' args f = f args

-- wrapper for haskell primitive functions
primitives :: [(String, [LispVal] -> LispVal)]
primitives =
    [ ("+"        , basicFuncWrapper (+))
    , ("-"        , basicFuncWrapper (-))
    , ("*"        , basicFuncWrapper (*))
    , ("/"        , basicFuncWrapper div)
    , ("mod"      , basicFuncWrapper mod)
    , ("quotient" , basicFuncWrapper quot)
    , ("remainder", basicFuncWrapper rem)
    , ("symbol?"  , unaryFuncWrapper symbol')
    , ("number?"  , unaryFuncWrapper number')
    , ("string?"  , unaryFuncWrapper string')
    , ("bool?"    , unaryFuncWrapper bool')
    ]

-- wrap primitive functions of haskell 
basicFuncWrapper :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
basicFuncWrapper func args = Number $ foldl1 func params
    where params = map unwrapNum args

-- wrap basic unary functions(like symbol?)
unaryFuncWrapper :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryFuncWrapper func [arg] = func arg

-- unwrap number from LispVal
unwrapNum :: LispVal -> Integer
unwrapNum (Number x) = x
unwrapNum _          = 0

symbol' :: LispVal -> LispVal
symbol' (Atom _) = Bool True
symbol' _        = Bool False

number' :: LispVal -> LispVal
number' (Number _) = Bool True
number' _          = Bool False

string' :: LispVal -> LispVal
string' (String _) = Bool True
string' _          = Bool False

bool' :: LispVal -> LispVal
bool' (Bool _) = Bool True
bool' _        = Bool False
