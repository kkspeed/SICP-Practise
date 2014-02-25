module Eval where
import Error
import Control.Monad.Error
import AST


eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Float _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", p, conseq, alt]) =
    do result <- eval p
       case result of
         Bool False -> eval alt
         _          -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                  ($ args)
                  (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop div),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolOp (==)),
              ("<", numBoolOp (<)),
              (">", numBoolOp (>)),
              ("/=", numBoolOp (/=)),
              (">=", numBoolOp (>=)),
              ("<=", numBoolOp (<=)),
              ("&&", boolBoolOp (&&)),
              ("||", boolBoolOp (||)),
              ("string=?", strBoolOp (==)),
              ("string>?", strBoolOp (>)),
              ("string>=?", strBoolOp (>=)),
              ("string<=?", strBoolOp (<=)),
              ("string<", strBoolOp (<)),
              ("symbol?", booleanSingletonOp isSymbol),
              ("string?", booleanSingletonOp isString),
              ("number?", booleanSingletonOp isNumber),
              ("car", car),
              ("cdr", cdr)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_]  = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolOp :: (Eq a) => (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolOp unpacker op args = if length args <= 1
                          then throwError $ NumArgs 2 args
                          else mapM unpacker args >>= \ps ->
                          return . Bool . all (\(a,b)->(a `op` b)) $ zip ps (tail ps)

numBoolOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolOp = boolOp unpackNum

strBoolOp :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolOp = boolOp unpackStr

boolBoolOp :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolOp = boolOp unpackBool

booleanSingletonOp :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
booleanSingletonOp op [x] = return . Bool $ op x
booleanSingletonOp _ vals = throwError $ NumArgs 1 vals

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                       if null parsed
                       then throwError $ TypeMismatch "number" $ String n
                       else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

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
