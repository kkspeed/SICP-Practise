module AST where
import Text.Parsec.Error
import Data.IORef
import Control.Monad.Error
import System.IO

type IOThrowsError = ErrorT LispError IO

type Env = IORef [(String, IORef LispVal)]

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Float Float
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String),
                     body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordList contents ++ ")"
showVal (DottedList hd tl) = "(" ++ unwordList hd ++ " . " ++ showVal tl ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = _, closure = _}) =
    "(lambda (" ++ unwords (map show args) ++
                (case varargs of
                   Nothing -> ""
                   Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

instance Show LispVal where
    show = showVal

unwordList :: [LispVal] -> String
unwordList = unwords . map showVal

type ThrowsError = Either LispError
