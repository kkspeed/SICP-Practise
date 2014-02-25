module AST where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Float Float

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordList contents ++ ")"
showVal (DottedList hd tl) = "(" ++ unwordList hd ++ " . " ++ showVal tl ++ ")"

instance Show LispVal where
    show = showVal

unwordList :: [LispVal] -> String
unwordList = unwords . map showVal
