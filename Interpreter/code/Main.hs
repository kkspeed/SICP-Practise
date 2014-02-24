module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric(readHex, readOct)
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
                return $let atom = [first] ++ rest
                        in case atom of
                             "#t" -> Bool True
                             "#f" -> Bool False
                             _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= \x -> return (Number $ read x)

parseDecimal :: Parser LispVal
parseDecimal = liftM (Number . read) $ many1 digit

parseHex :: Parser LispVal
-- parseHex = string "#x" >> do
--              x <- many1 (hexDigit <|> digit)
--              let ((d, _):_) = readHex x
--              return $ Number d

-- parseHex = string "#x" >> many1 (hexDigit <|> digit) >>= \x ->
--            return (readHex x) >>= \((d, _):_) -> return $ Number d

parseHex = string "#x" >> many1 (hexDigit <|> digit) >>= \x ->
           return (readHex x) >>= \((d, _):_) -> return $ Number d

parseOct :: Parser LispVal
parseOct = do
  val <- many1 (octDigit <|> digit)
  let ((d, _):_) = readOct val
  return $ Number d

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber
