module Eval where

import AST
import qualified Data.Map as Map
import Data.Map (Map)

-- Environment to store variable bindings
type Env = Map String Expr

-- Pattern matching for expressions
matchPattern :: Pattern -> Expr -> Bool
matchPattern PWild _ = True
matchPattern (Plit k) (Lit n) = k == n
matchPattern (PVar _) _ = True
matchPattern PNil (List []) = True
matchPattern (PCons p1 p2) (Cons h t) = matchPattern p1 h && matchPattern p2 t
matchPattern (PCons p1 p2) (List (x:xs)) = matchPattern p1 x && matchPattern p2 (List xs)
matchPattern _ _ = False

-- Substitute variable 'x' with 'arg' in 'body'
substitute :: String -> Expr -> Expr -> Expr
substitute x arg = go
  where
    go (Var y) | x == y = arg
    go (Lamba y body) | x /= y = Lamba y (go body) 
    go (Apply e1 e2) = Apply (go e1) (go e2)
    go (List es) = List (map go es)
    go (Cons e1 e2) = Cons (go e1) (go e2)
    go (Case scrut branches) = Case (go scrut) [(p, go e) | (p, e) <- branches]
    go (Mu f body) = Mu f (go body)
    go other = other  -- Literals, etc.

-- Evaluate a list of expressions with an environment
evalProgram :: [Expr] -> Expr
evalProgram exprs = case exprs of
  [] -> Lit 0  -- Default value for empty program
  [expr] -> eval Map.empty expr  -- Single expression
  (expr:rest) -> case expr of
    Let name value _ -> 
      let env = Map.singleton name (eval Map.empty value)
      in evalWithEnv env rest
    _ -> evalWithEnv Map.empty exprs  -- No definitions, just evaluate all

-- Evaluate a list of expressions with an environment
evalWithEnv :: Env -> [Expr] -> Expr
evalWithEnv _ [] = Lit 0  -- Default value for empty program
evalWithEnv env [expr] = eval env expr  -- Last expression
evalWithEnv env (expr:rest) = case expr of
  Let name value _ -> 
    let newValue = eval env value
        newEnv = Map.insert name newValue env
    in evalWithEnv newEnv rest
  _ -> evalWithEnv env rest  -- Evaluate and continue

-- Evaluate an expression with an environment
eval :: Env -> Expr -> Expr
eval env (Var name) = case Map.lookup name env of
  Just value -> value
  Nothing -> Var name  -- Unbound variable

eval env (Apply e1 e2) = case eval env e1 of
  Lamba x body -> eval (Map.insert x (eval env e2) env) body
  f -> Apply f (eval env e2)

eval env (Mu f body) = 
  let recursiveExpr = Mu f body
      envWithF = Map.insert f recursiveExpr env
  in eval envWithF body

eval env (Let name value body) =
  let valueEval = eval env value
      envWithBinding = Map.insert name valueEval env
  in eval envWithBinding body

eval env (BinOp op e1 e2) = 
  case (eval env e1, eval env e2) of
    (Lit v1, Lit v2) -> 
      case op of
        Add -> Lit (v1 + v2)
        Sub -> Lit (v1 - v2)
        Mul -> Lit (v1 * v2)
        Div -> if v2 /= 0 
               then Lit (v1 `div` v2) 
               else error "Division by zero"
        Eq  -> if v1 == v2 then Lit 1 else Lit 0
        Neq -> if v1 /= v2 then Lit 1 else Lit 0
        Lt  -> if v1 < v2  then Lit 1 else Lit 0
        Gt  -> if v1 > v2  then Lit 1 else Lit 0
    _ -> error "Binary operation requires numeric values"

eval env (List es) = List (map (eval env) es)

eval env (Cons e1 e2) = 
  let h = eval env e1
      t = eval env e2
  in case t of
      List es -> List (h : es)  -- Cons to a list
      _       -> Cons h t       -- Keep as Cons if tail is not a list

-- Evaluate case expressions
eval env (Case scrut branches) = 
  let scrutVal = eval env scrut
  in case [(p, e) | (p, e) <- branches, matchPattern p scrutVal] of
       ((_, e):_) -> eval env e
       [] -> error $ "No matching pattern found for: " ++ show scrutVal

-- Simple expressions pass through unchanged
eval _ (Lit n) = Lit n
eval _ (Str s) = Str s
eval _ other = other
