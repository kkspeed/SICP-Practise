module Eval where

import AST

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Float _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop div),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", booleanSingletonOp isSymbol),
              ("string?", booleanSingletonOp isString),
              ("number?", booleanSingletonOp isNumber)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

booleanSingletonOp :: (LispVal -> Bool) -> [LispVal] -> LispVal
booleanSingletonOp op params = Bool $ and $ map op params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
                       if null parsed
                       then 0
                       else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _        = False

isString :: LispVal -> Bool
isString (String _) = True
isString _          = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber (Float _)  = True
isNumber _          = False
