module Main where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

-- Define parser type (using String input for simplicity)
type Parser = Parsec Void String

-- Test parser: matches 'a'
testParser :: Parser Char
testParser = char 'a'

main :: IO ()
main = do
  let input = "a"
  case parse testParser "" input of
    Right c -> putStrLn $ "Success: " ++ show c
    Left err -> putStrLn $ "Error: " ++ errorBundlePretty err
