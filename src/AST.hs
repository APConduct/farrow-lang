module AST where

    data Expr
        = Var String
        | Lit Int
        | Str String
        | Lamba String Expr
        | Apply Expr Expr
        | BinOp BinOp Expr Expr
        | Case Expr [(Pattern, Expr)]
        | Mu String Expr
        | Let String Expr Expr
        deriving (Show, Eq)
    data BinOp
        = Add | Sub | Mul | Div
        | Eq | Neq | Lt | Gt
        deriving (Show, Eq)
    data Pattern
        = PVar String
        | Plit Int
        | PWild
        | PCons Pattern Pattern
        deriving (Show, Eq)