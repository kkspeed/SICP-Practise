module Error where
import Control.Monad.Error
import Text.Parsec.Error
import AST

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ":" ++ varname
showError (BadSpecialForm message form) = message ++ ":" ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args: " ++
                                     "found values " ++ unwordList found
showError (TypeMismatch expected found) = "Invalid type expected " ++ expected ++
                                          ", found " ++ show found
showError (Parser parseError) = "Error during parsing: " ++ show parseError

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default



trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
