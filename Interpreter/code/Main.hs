module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric(readHex, readOct, readFloat)
import Control.Monad

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces  = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> show val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Float Float
             deriving (Show)

escapeChar :: Parser Char
escapeChar =  char '\\' >> oneOf "\"tnr\\" >>= \x ->
              return $ case lookup x [('n', '\n'), ('t', '\t')] of
                         Just v -> v
                         Nothing -> x

parseString :: Parser LispVal
parseString = char '"' >> many (escapeChar <|> noneOf "\"") >>= \x ->
              char '"' >> (return $ String x)

parseAtom :: Parser LispVal
parseAtom = (letter <|> symbol) >>= \first ->
            many (letter <|> digit <|> symbol) >>= \rest ->
                return $ let atom = [first] ++ rest
                         in case atom of
                              "#t" -> Bool True
                              "#f" -> Bool False
                              _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = try parseFloat <|> parseDecimal <|> try parseHex <|> parseOct

parseDecimal :: Parser LispVal
parseDecimal = liftM (Number . read) $ many1 digit

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  let ((d,_):_) = readFloat $ x ++ "." ++ y
  return $ Float d

parseHex :: Parser LispVal
parseHex = string "#x" >> many1 (hexDigit <|> digit) >>= return . readHex >>=
           \((d, _):_) -> return $ Number d

parseOct :: Parser LispVal
parseOct = string "#o" >> many1 (octDigit <|> digit) >>= return . readOct >>=
           \((d, _):_) -> return $ Number d

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuote :: Parser LispVal
parseQuasiQuote = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> try parseNumber <|> parseAtom <|> parseString
            <|> parseQuoted <|> do char '('
                                   x <- (try parseList) <|> parseDottedList
                                   char ')'
                                   return x
