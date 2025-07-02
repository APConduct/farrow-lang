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
  Lit n -> case [ e | (p, e) <- branches, matchPattern p (Lit n) ] of
            (e:_) -> eval e
            [] -> error "No matching pattern found"
  Var x -> error $ "Cannot match on unbound variable: " ++ x
  Str s -> error $ "Cannot match on string: " ++ s
  Lamba _ _ -> error "Cannot match on lambda"
  Apply _ _ -> error "Cannot match on application"
  BinOp _ _ _ -> error "Cannot match on binary operation"
  Case _ _ -> error "Cannot match on nested case"
  Mu _ _ -> error "Cannot match on recursive definition"
  Let _ _ _ -> error "Cannot match on let binding"
  where matchPattern PWild _ = True
        matchPattern (Plit k) (Lit n) = k == n
        matchPattern (PVar _) _ = True
        matchPattern (PCons p1 p2) _ = False  -- Implement for lists when you add them
        matchPattern _ _ = False
-- Evaluate binary operations
eval (BinOp op e1 e2) = 
  case (eval e1, eval e2) of
    (Lit v1, Lit v2) -> 
      case op of
        Add -> Lit (v1 + v2)
        Sub -> Lit (v1 - v2)
        Mul -> Lit (v1 * v2)
        Div -> if v2 /= 0 
               then Lit (v1 `div` v2) 
               else error "Division by zero"
        Eq  -> if v1 == v2 then Lit 1 else Lit 0  -- Using 1/0 for true/false
        Neq -> if v1 /= v2 then Lit 1 else Lit 0
        Lt  -> if v1 < v2  then Lit 1 else Lit 0
        Gt  -> if v1 > v2  then Lit 1 else Lit 0
    _ -> error "Binary operation requires numeric values"

-- Let expressions
eval (Let name value body) = 
  eval (substitute name (eval value) body)

eval other = other  -- Variables, literals
