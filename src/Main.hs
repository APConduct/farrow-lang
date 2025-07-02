module Main where

import AST
import Parser (parseExpr)
import Eval (eval)
import System.Console.Haskeline
import Text.Megaparsec (parse, errorBundlePretty)

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      input <- getInputLine "F↦> "  -- Changed from getInput to getInputLine
      case input of
        Nothing -> return ()  -- Ctrl-D
        Just ":q" -> return ()  -- Quit
        Just line -> do
          case parse parseExpr "" line of
            Right expr -> outputStrLn $ show (eval expr)
            Left err -> outputStrLn $ "Error: " ++ errorBundlePretty err
          loop

main :: IO ()
main = do
  putStrLn "F↦ v0.1 - Ctrl-D to quit"
  repl
