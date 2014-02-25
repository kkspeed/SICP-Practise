module Main where
import System.Environment
import Parse
import Eval

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
