module Main where

import AST
import Parser (parseExpr)
import Eval (eval)
import System.Console.Haskeline
import Text.Megaparsec (parse, errorBundlePretty)
import System.Environment (getArgs)
import System.IO (readFile)
import Control.Monad.IO.Class (liftIO)

-- Evaluate a string expression
evalString :: String -> Either String Expr
evalString input = case parse parseExpr "" input of
  Right expr -> Right (eval expr)
  Left err -> Left (errorBundlePretty err)

-- Load and evaluate a file
evalFile :: FilePath -> IO ()
evalFile filePath = do
  contents <- readFile filePath
  case evalString contents of
    Right result -> putStrLn $ "Result: " ++ show result
    Left err -> putStrLn $ "Error: " ++ err

-- Interactive REPL
repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      input <- getInputLine "F↦> "
      case input of
        Nothing -> return ()  -- Ctrl-D
        Just ":q" -> return ()  -- Quit
        Just (':':'l':' ':filePath) -> do  -- Load file command
          liftIO $ evalFile filePath
          loop
        Just line -> do
          case evalString line of
            Right result -> outputStrLn $ show result
            Left err -> outputStrLn $ "Error: " ++ err
          loop

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> evalFile filePath  -- Run a file if provided
    _ -> do
      putStrLn "F↦ v0.1 - Ctrl-D to quit, :l file to load a file"
      repl
