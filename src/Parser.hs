module Parser where
    import Text.Megaparsec
    import Text.Megaparsec.Char
    import qualified Text.Megaparsec.Char.Lexer as L
    import AST

    type Parser = Parsec Void String

    -- Helper: Parse a symbol
    symbol :: String -> Parser String
    symbol = L.symbol sc

    -- Helper: Skip whitespace
    sc :: Parser ()
    sc = L.space space1
        (L.skipLineComment "--")
        (L.skipBlockComment "{-" "-}")

    -- Top level parser
    parseExpr :: Parser Expr
    parseExpr = do 
        sc
        expr <- choice [try letExpr, try muExpr, try lambaExpr, try caseExpr, try binOpExpr]
        sc
        return expr

    -- Parse a lambda expression
    lambaExpr :: Parser Expr
    lambaExpr = do
        var <- some letterChar
        symbol "|->"
        body <- parseExpr
        return $ Lamba var body

    -- Parse a pipe (x -> f -> g)
    pipeExpr :: Parser Expr
    pipeExpr = chainl1 term (symbol "->" >> return Apply)

    -- Parse a let-binding (f := x |-> x + 1)
    letExpr :: Parser Expr
    letExpr = do
        name <- some letterChar
        symbol ":="
        value <- parseExpr
        return $ Let name value

    -- Parse µ-recursion (µ f |-> n ...)
    muExpr :: Parser Expr
    muExpr = do
        symbol "µ"
        f <- some letterChar
        symbol "|->"
        body <- parseExpr
        return $ Mu f body

    -- Parse a case expression (case x of 0 => 1 | _ => 2)
    caseExpr :: Parser Expr
    caseExpr = do
        symbol "case"
        scrutinee <- parseExpr
        symbol "of"
        patterns <- sepBy1 branch (symbol "|")
        return $ Case scrutinee patterns
        where
            branch = do
                pat <- pattern
                symbol "=>"
                expr <- parseExpr
                return (pat, expr)

    -- Parse a pattern (0, x, _)
    pattern :: Parser Pattern
    pattern = choice
        [ PWild <$ symbol "_"
        , PLit <$> L.decimal
        , PVar <$> some letterChar
        , PCons <$> pattern <*> (symbol ":" *> pattern)
        ]

    -- Parse binary ops (x + y)
    binOpExpr :: Parser Expr
    binOpExpr = makeExprParser term operatorTable

    term :: Parser Expr
    term = choice
        [ Lit <$> L.decimal
        , Str <$> (char '"' *> manyTill L.charLiteral (char '"'))
        , Var <$> some letterChar
        , between (symbol "(") (symbol ")") parseExpr
        ]

    operatorTable :: [[Operator Parser Expr]]
    operatorTable =
        [ 
            [ 
                InfixL (BinOp Mul <$ symbol "*")
                , InfixL (BinOp Div <$ symbol "/"),
            ]
            [
                InfixL (BinOp Add <$ symbol "+")
                , InfixL (BinOp Sub <$ symbol "-")
            ]
            [
                InfixR (BinOp Eq <$ symbol "=")
                , InfixR (BinOp Neq <$ symbol "!=")
            ]
        ]