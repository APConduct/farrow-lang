module Parser where

import AST
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Helper: Skip whitespace
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

-- Helper: Parse a lexeme (token with trailing space)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Helper: Parse a specific string as a token
symbol :: String -> Parser String
symbol = L.symbol sc

-- Parse a program (multiple expressions)
parseProgram :: Parser [Expr]
parseProgram = do
  sc
  exprs <- sepEndBy parseExpr sc
  eof
  return exprs

-- Top level parser
parseExpr :: Parser Expr
parseExpr = choice
  [ try defExpr        -- Try variable definitions first
  , try binOpExpr      -- Then binary operations
  , try appExpr        -- Then function applications
  , try lambaExpr      -- Then lambda expressions
  , try muExpr         -- Then mu recursion
  , try caseExpr       -- Then case expressions
  , try parseList      -- Then list literals
  , term               -- Finally just basic terms
  ]

-- Parse a definition (name := expr)
defExpr :: Parser Expr
defExpr = do
  name <- lexeme (some letterChar)
  _ <- string ":"     -- Parse colon directly
  _ <- string "="     -- Parse equals directly
  sc                  -- Skip whitespace after :=
  value <- choice [try (Lit <$> L.decimal), parseExpr]  -- Try to parse a literal first
  return $ Let name value (Var name)



-- Parse binary operations
binOpExpr :: Parser Expr
binOpExpr = makeExprParser term operatorTable

-- Parse a term (atomic expression)
term :: Parser Expr
term = choice
  [ Lit <$> lexeme L.decimal                  -- Parse numbers first
  , between (symbol "(") (symbol ")") parseExpr  -- Then parenthesized expressions
  , Var <$> lexeme (some letterChar)          -- Then variables
  ]


-- Define operator precedence and associativity
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (BinOp Mul <$ symbol "*")
    , InfixL (BinOp Div <$ symbol "/")
    ]
  , [ InfixL (BinOp Add <$ symbol "+")
    , InfixL (BinOp Sub <$ symbol "-")
    ]
  , [ InfixR (BinOp Eq <$ symbol "=")
    , InfixR (BinOp Neq <$ symbol "!=")
    ]
  ]

-- Parse function application (f arg1 arg2)
appExpr :: Parser Expr
appExpr = do
  func <- term
  args <- many term  -- Changed from some to many
  if null args then fail "Not an application" else return $ foldl Apply func args


-- Parse a lambda expression
lambaExpr :: Parser Expr
lambaExpr = do
  _ <- symbol "λ" <|> symbol "\\"  -- Allow both λ and \
  var <- lexeme (some letterChar)
  _ <- symbol "->"
  body <- parseExpr
  return $ Lamba var body

-- Parse µ-recursion (µ f -> body)
muExpr :: Parser Expr
muExpr = do
  _ <- symbol "µ"
  f <- lexeme (some letterChar)
  _ <- symbol "->"
  body <- parseExpr
  return $ Mu f body

-- Parse a case expression
caseExpr :: Parser Expr
caseExpr = do
  _ <- symbol "case"
  scrutinee <- parseExpr
  _ <- symbol "of"
  patterns <- sepBy1 branch (symbol "|")
  return $ Case scrutinee patterns
  where
    branch = do
      pat <- parsePattern
      _ <- symbol "=>"
      expr <- parseExpr
      return (pat, expr)

-- Parse a pattern
parsePattern :: Parser Pattern
parsePattern = choice
  [ PWild <$ symbol "_"
  , Plit <$> lexeme L.decimal
  , PNil <$ symbol "[]"
  , try (PCons <$> parsePattern <*> (symbol ":" *> parsePattern))
  , PVar <$> lexeme (some letterChar)
  ]

-- Parse a list literal
parseList :: Parser Expr
parseList = do
  _ <- symbol "["
  elements <- sepBy parseExpr (symbol ",")
  _ <- symbol "]"
  return $ List elements
