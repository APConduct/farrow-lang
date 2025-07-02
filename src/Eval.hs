-- src/Eval.hs
module Eval where

import AST

-- Substitute variable 'x' with 'arg' in 'body'
substitute :: String -> Expr -> Expr -> Expr
substitute x arg = go
  where
    go (Var y) | x == y = arg
    go (Lamba y body) | x /= y = Lamba y (go body)  -- Changed Lambda to Lamba
    go (Apply e1 e2) = Apply (go e1) (go e2)
    go (Case scrut branches) = Case (go scrut) [(p, go e) | (p, e) <- branches]
    go (Mu f body) = Mu f (go body)
    go other = other  -- Literals, etc.

-- Evaluate an expression to normal form
eval :: Expr -> Expr
eval (Apply e1 e2) = case eval e1 of
  Lamba x body -> eval (substitute x (eval e2) body)  -- Changed Lambda to Lamba
  f -> Apply f (eval e2)
eval (Mu f body) = eval (substitute f (Mu f body) body)
eval (Case scrut branches) = case eval scrut of
  Lit n -> eval $ snd (head [ (p, e) | (p, e) <- branches, match p n ])
  where match (Plit k) n = k == n  -- Changed PLit to Plit
        match PWild _ = True
eval other = other  -- Variables, literals
