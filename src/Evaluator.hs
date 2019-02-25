module Evaluator where

import Parser

-- lisp ADT
-- data LispVal = Atom String                      -- atom
--              | List [LispVal]                   -- (x y z)
--              | DottedList [LispVal] LispVal     -- (x y . z)
--              | Number Integer                   -- 1
--              | String String                    -- "this is string"
--              | Bool Bool                        -- #t / #f
--              | Character Char                   -- 'a'
--              | Float Double                     -- 1.22
--              | Ratio Rational                   -- 1/2  
--              | Vector (Array Int LispVal)       -- #(1 2 3)


-- evaluator 
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval val@(Float _) = val
eval val@(Ratio _) = val
eval (List [Atom "quoted", val]) = val