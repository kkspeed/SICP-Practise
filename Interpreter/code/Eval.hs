{-# LANGUAGE ExistentialQuantification, DoAndIfThenElse, RankNTypes #-}
module Eval where
import Error
import Control.Monad.Error
import System.IO
import System.Random (randomRIO)
import Data.IORef
import Data.Time.Clock.POSIX (getPOSIXTime)
import Parse
import AST

nullEnv :: IO Env
nullEnv = newIORef []



liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar
                                   "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do alreadyDefined <- liftIO $ isBound envRef var
                                if alreadyDefined
                                then setVar envRef var value >> return value
                                else liftIO $ do
                                  valueRef <- newIORef value
                                  env <- readIORef envRef
                                  writeIORef envRef ((var, valueRef) : env)
                                  return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bds env = liftM (++ env) (mapM addBinding bds)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Float _) = return val
eval env (Atom i) = getVar env i
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda": List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List [Atom "if", p, conseq, alt]) =
    do result <- eval env p
       case result of
         Bool False -> eval env alt
         _          -> eval env conseq
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
-- Exercise 5-3: Implement cond
eval env (List ((Atom "cond"):pls)) = evalCondList pls
      where evalCondList (x:xs) =
                case x of
                  (List [p, f]) -> do result <- eval env p
                                      case result of
                                        Bool False -> evalCondList xs
                                        _          -> eval env f
                  _              -> throwError $ BadSpecialForm "Malformed condition form" x
eval env (List ((Atom "let"):varbinds:body)) =
    eval env $ List $ [List $ [Atom "lambda", List (vars varbinds)] ++ body]++
             (args varbinds)
    where vars (List bds) = map (\(List (x:_)) -> x) bds
          args (List bds) = join $ map (\(List (_:v)) -> v) bds
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals

eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params vararg body closure) args =
    if num params /= num args && vararg == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>=
         bindVarArgs vararg >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env =
              case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env
apply v _ = throwError $ BadSpecialForm "Unrecognized function" v

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFun IOFunc) ioPrimitives
                                      ++ map (makeFun PrimitiveFunc) primitives)
    where makeFun constructor (var, func) = (var, constructor func)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numOpList $ doNumeric (+)),
              ("-", numOpList $ doNumeric (-)),
              ("*", numOpList $ doNumeric (*)),
              ("/", numOpList doDiv),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", boolOpList $ doComparison (==)),
              ("<", boolOpList $ doComparison (<)),
              (">", boolOpList $ doComparison (>)),
              ("/=", boolOpList $ doComparison (/=)),
              (">=", boolOpList $ doComparison (>=)),
              ("<=", boolOpList $ doComparison (<=)),
              ("and", boolBoolOp (&&)),
              ("or", boolBoolOp (||)),
              ("string=?", strBoolOp (==)),
              ("string>?", strBoolOp (>)),
              ("string>=?", strBoolOp (>=)),
              ("string<=?", strBoolOp (<=)),
              ("string<", strBoolOp (<)),
              ("symbol?", booleanSingletonOp isSymbol),
              ("string?", booleanSingletonOp isString),
              ("number?", booleanSingletonOp isNumber),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("list", list),
              ("pair?", pair),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]


numOpList :: (LispVal -> LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
numOpList _  p@[_]    = throwError $ NumArgs 2 p
numOpList _  p@[]    = throwError $ NumArgs 2 p
numOpList op (p:ps) = foldM op p ps

boolOpList :: (LispVal -> LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
boolOpList op ps = if length ps <= 1
                   then throwError $ NumArgs 2 ps
                   else return . Bool . all (\(a,b)->(a `op` b)) $ zip ps (tail ps)

doNumeric :: (forall a . (Num a) => (a -> a -> a)) -> LispVal -> LispVal -> ThrowsError LispVal
doNumeric op (Number a) (Number b) = return $ Number $ a `op` b
doNumeric op (Number a) (Float b) = return $ Float $ fromIntegral a `op` b
doNumeric op (Float a) (Number b) = return $ Float $ a `op` fromIntegral b
doNumeric op (Float a) (Float b) = return $ Float $ a `op` b

doDiv :: LispVal -> LispVal -> ThrowsError LispVal
doDiv (Number a) (Number b) = return $ Number $ a `div` b
doDiv (Float a) (Float b)   = return $ Float $ a / b
doDiv (Float a) (Number b)  = return $ Float (a / fromIntegral b)
doDiv (Number a) (Float b)  = return $ Float (fromIntegral a / b)

doComparison :: (forall a . (Ord a) => (a -> a -> Bool)) -> LispVal -> LispVal -> Bool
doComparison op (Number a) (Number b) = a `op` b
doComparison op (Float a) (Float b) = a `op` b
doComparison op (Float a) (Number b) = a `op` fromIntegral b
doComparison op (Number a) (Float b) = fromIntegral a `op` b

numBinOp :: [LispVal] -> ThrowsError LispVal
numBinOp [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                    [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_]  = throwError $ NumArgs 2 singleVal
numericBinop op pms = mapM unpackNum pms >>= return . Number . foldl1 op

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

unpackNum ::  LispVal -> ThrowsError Integer
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

unpackFloat :: LispVal -> ThrowsError Float
unpackFloat (Float f) = return f
unpackFloat notFloat = throwError $ TypeMismatch "float" notFloat

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List lst] = return . List $ x:lst
cons [x, DottedList lst a] = return $ DottedList (x:lst) a
cons [x, y] = return . DottedList [x] $ y
cons (m:_) = throwError $ BadSpecialForm "cons" m

list :: [LispVal] -> ThrowsError LispVal
list lst = return $ List $ lst

pair :: [LispVal] -> ThrowsError LispVal
pair [List _] = return $ Bool True
pair [DottedList _ _] = return $ Bool True
pair _ = return $ Bool False

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x],
                                                  List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                 (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left _ -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do unpacked1 <- unpacker arg1
       unpacked2 <- unpacker arg2
       return $ unpacked1 == unpacked2
       `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                 (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case equal [x1, x2] of
                               Left _ -> False
                               Right (Bool val) -> val
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                    [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)

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

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll),
                ("random", randomNum),
                ("runtime", runtime),
                ("display", display),
                ("newline", newline)]

randomNum :: [LispVal] -> IOThrowsError LispVal
randomNum [Number n] = liftIO $ randomRIO (0, n-1) >>= return . Number
randomNum [Float n] = liftIO $ randomRIO (0, n-1) >>= return . Float
randomNum [v] = throwError $ TypeMismatch "Expect Int or Float" v
randomNum args   = throwError $ NumArgs 1 args

runtime :: [LispVal] -> IOThrowsError LispVal
runtime [] = liftIO $ getPOSIXTime >>= return . Number . round
runtime args = throwError $ NumArgs 0 args

display :: [LispVal] -> IOThrowsError LispVal
display args = mapM (\x -> writeProc [x]) args >> (return $ Bool True)

newline :: [LispVal] -> IOThrowsError LispVal
newline [] = writeNewLine [Port stdout]
newline args = throwError $ NumArgs 0 args

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $
                                  openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPutStr port (show obj) >> (return $ Bool True)

writeNewLine :: [LispVal] -> IOThrowsError LispVal
writeNewLine []  = writeNewLine [Port stdout]
writeNewLine [Port port] = liftIO $ hPutStrLn port "" >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
