use crate::ast::{BinOp, Expr, Literal, Pattern, SpannedExpr, SpannedPattern, UnaryOp};
use crate::environment::{CallFrame, CallStack, Environment, Value};
use crate::error::{RuntimeError, RuntimeResult};

const MAX_RECURSION_DEPTH: usize = 1000;

/// The main evaluator for Farrow expressions
#[derive(Debug)]
pub struct Evaluator {
    call_stack: CallStack,
}

impl Evaluator {
    /// Create a new evaluator
    pub fn new() -> Self {
        Evaluator {
            call_stack: CallStack::new(MAX_RECURSION_DEPTH),
        }
    }

    /// Evaluate an expression in the given environment
    pub fn eval(&mut self, env: &Environment, expr: &SpannedExpr) -> RuntimeResult<Value> {
        self.eval_expr(env, expr)
    }

    /// Main expression evaluation function
    fn eval_expr(&mut self, env: &Environment, expr: &SpannedExpr) -> RuntimeResult<Value> {
        match &expr.node {
            Expr::Lit(lit) => self.eval_literal(lit),
            Expr::Var(name) => env.lookup(name),
            Expr::Lambda { param, body } => Ok(Value::Function {
                param: param.clone(),
                body: (**body).clone(),
                env: env.clone(),
            }),
            Expr::Mu { name, body } => self.eval_mu(env, name, body),
            Expr::Apply { func, arg } => self.eval_application(env, func, arg),
            Expr::BinOp { op, lhs, rhs } => self.eval_binop(env, op, lhs, rhs),
            Expr::UnaryOp { op, operand } => self.eval_unaryop(env, op, operand),
            Expr::Let { name, value, body } => self.eval_let(env, name, value, body),
            Expr::Case {
                scrutinee,
                branches,
            } => self.eval_case(env, scrutinee, branches),
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => self.eval_if(env, condition, then_branch, else_branch),
            Expr::List(elements) => self.eval_list(env, elements),
            Expr::Cons { head, tail } => self.eval_cons(env, head, tail),
            Expr::Block(exprs) => self.eval_block(env, exprs),
        }
    }

    /// Evaluate a literal value
    fn eval_literal(&self, lit: &Literal) -> RuntimeResult<Value> {
        match lit {
            Literal::Int(n) => Ok(Value::Int(*n)),
            Literal::Float(f) => Ok(Value::Float(*f)),
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Bool(b) => Ok(Value::Bool(*b)),
            Literal::Unit => Ok(Value::Unit),
        }
    }

    /// Evaluate a mu (recursive function) expression
    fn eval_mu(&self, env: &Environment, name: &str, body: &SpannedExpr) -> RuntimeResult<Value> {
        match &body.node {
            Expr::Lambda { param, body } => Ok(Value::RecFunction {
                name: name.to_string(),
                param: param.clone(),
                body: (**body).clone(),
                env: env.clone(),
            }),
            _ => Err(RuntimeError::type_mismatch(
                "lambda expression",
                "other expression",
            )),
        }
    }

    /// Evaluate function application
    fn eval_application(
        &mut self,
        env: &Environment,
        func: &SpannedExpr,
        arg: &SpannedExpr,
    ) -> RuntimeResult<Value> {
        let func_val = self.eval_expr(env, func)?;
        let arg_val = self.eval_expr(env, arg)?;

        match func_val {
            Value::Function {
                param,
                body,
                env: func_env,
            } => {
                // Create new environment for function body
                let new_env = func_env.extend();
                new_env.define(param.clone(), arg_val);

                // Push call frame
                let frame = CallFrame::anonymous(Some(func.span.clone()), new_env.depth());
                self.call_stack.push(frame)?;

                // Evaluate function body
                let result = self.eval_expr(&new_env, &body);

                // Pop call frame
                self.call_stack.pop();

                result
            }
            Value::RecFunction {
                name,
                param,
                body,
                env: func_env,
            } => {
                // Create new environment for recursive function
                let new_env = func_env.extend();

                // Add the recursive function itself to the environment
                new_env.define(
                    name.clone(),
                    Value::RecFunction {
                        name: name.clone(),
                        param: param.clone(),
                        body: body.clone(),
                        env: func_env,
                    },
                );
                new_env.define(param, arg_val);

                // Push call frame with function name
                let frame = CallFrame::named(name, Some(func.span.clone()), new_env.depth());
                self.call_stack.push(frame)?;

                // Evaluate function body
                let result = self.eval_expr(&new_env, &body);

                // Pop call frame
                self.call_stack.pop();

                result
            }
            Value::BuiltinFunction(name) => self.eval_builtin(&name, arg_val),
            _ => Err(RuntimeError::invalid_application(func_val.type_name())),
        }
    }

    /// Evaluate binary operations
    fn eval_binop(
        &mut self,
        env: &Environment,
        op: &BinOp,
        lhs: &SpannedExpr,
        rhs: &SpannedExpr,
    ) -> RuntimeResult<Value> {
        match op {
            // Short-circuiting logical operators
            BinOp::And => {
                let left_val = self.eval_expr(env, lhs)?;
                if !left_val.is_truthy() {
                    Ok(Value::Bool(false))
                } else {
                    let right_val = self.eval_expr(env, rhs)?;
                    Ok(Value::Bool(right_val.is_truthy()))
                }
            }
            BinOp::Or => {
                let left_val = self.eval_expr(env, lhs)?;
                if left_val.is_truthy() {
                    Ok(Value::Bool(true))
                } else {
                    let right_val = self.eval_expr(env, rhs)?;
                    Ok(Value::Bool(right_val.is_truthy()))
                }
            }
            // Pipe operator
            BinOp::Pipe => {
                let left_val = self.eval_expr(env, lhs)?;
                match self.eval_expr(env, rhs)? {
                    Value::Function {
                        param,
                        body,
                        env: func_env,
                    } => {
                        let new_env = func_env.extend();
                        new_env.define(param, left_val);
                        self.eval_expr(&new_env, &body)
                    }
                    Value::BuiltinFunction(name) => self.eval_builtin(&name, left_val),
                    func_val => Err(RuntimeError::invalid_application(func_val.type_name())),
                }
            }
            // Regular binary operators
            _ => {
                let left_val = self.eval_expr(env, lhs)?;
                let right_val = self.eval_expr(env, rhs)?;
                self.apply_binop(op, &left_val, &right_val)
            }
        }
    }

    /// Apply a binary operator to two values
    fn apply_binop(&self, op: &BinOp, left: &Value, right: &Value) -> RuntimeResult<Value> {
        match (op, left, right) {
            // Arithmetic operations
            (BinOp::Add, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (BinOp::Add, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (BinOp::Add, Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
            (BinOp::Add, Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + *b as f64)),
            (BinOp::Add, Value::String(a), Value::String(b)) => {
                Ok(Value::String(format!("{}{}", a, b)))
            }

            (BinOp::Sub, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (BinOp::Sub, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (BinOp::Sub, Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
            (BinOp::Sub, Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - *b as f64)),

            (BinOp::Mul, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (BinOp::Mul, Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (BinOp::Mul, Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 * b)),
            (BinOp::Mul, Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * *b as f64)),

            (BinOp::Div, Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(RuntimeError::division_by_zero())
                } else {
                    Ok(Value::Int(a / b))
                }
            }
            (BinOp::Div, Value::Float(a), Value::Float(b)) => {
                if *b == 0.0 {
                    Err(RuntimeError::division_by_zero())
                } else {
                    Ok(Value::Float(a / b))
                }
            }
            (BinOp::Div, Value::Int(a), Value::Float(b)) => {
                if *b == 0.0 {
                    Err(RuntimeError::division_by_zero())
                } else {
                    Ok(Value::Float(*a as f64 / b))
                }
            }
            (BinOp::Div, Value::Float(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(RuntimeError::division_by_zero())
                } else {
                    Ok(Value::Float(a / *b as f64))
                }
            }

            (BinOp::Mod, Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(RuntimeError::division_by_zero())
                } else {
                    Ok(Value::Int(a % b))
                }
            }

            // Comparison operations
            (BinOp::Eq, a, b) => Ok(Value::Bool(a == b)),
            (BinOp::Neq, a, b) => Ok(Value::Bool(a != b)),

            (BinOp::Lt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
            (BinOp::Lt, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
            (BinOp::Lt, Value::String(a), Value::String(b)) => Ok(Value::Bool(a < b)),

            (BinOp::Le, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
            (BinOp::Le, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
            (BinOp::Le, Value::String(a), Value::String(b)) => Ok(Value::Bool(a <= b)),

            (BinOp::Gt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
            (BinOp::Gt, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
            (BinOp::Gt, Value::String(a), Value::String(b)) => Ok(Value::Bool(a > b)),

            (BinOp::Ge, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
            (BinOp::Ge, Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
            (BinOp::Ge, Value::String(a), Value::String(b)) => Ok(Value::Bool(a >= b)),

            // List operations
            (BinOp::Append, Value::List(a), Value::List(b)) => {
                let mut result = a.clone();
                result.extend(b.clone());
                Ok(Value::List(result))
            }

            _ => Err(RuntimeError::type_mismatch(
                &format!("{} and {}", left.type_name(), right.type_name()),
                &format!("operands for {:?}", op),
            )),
        }
    }

    /// Evaluate unary operations
    fn eval_unaryop(
        &mut self,
        env: &Environment,
        op: &UnaryOp,
        operand: &SpannedExpr,
    ) -> RuntimeResult<Value> {
        let val = self.eval_expr(env, operand)?;
        match (op, &val) {
            (UnaryOp::Neg, Value::Int(n)) => Ok(Value::Int(-n)),
            (UnaryOp::Neg, Value::Float(f)) => Ok(Value::Float(-f)),
            (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
            _ => Err(RuntimeError::type_mismatch(
                &format!("operand for {:?}", op),
                val.type_name(),
            )),
        }
    }

    /// Evaluate let expressions
    fn eval_let(
        &mut self,
        env: &Environment,
        name: &str,
        value: &SpannedExpr,
        body: &SpannedExpr,
    ) -> RuntimeResult<Value> {
        let val = self.eval_expr(env, value)?;
        let new_env = env.extend();
        new_env.define(name.to_string(), val);
        self.eval_expr(&new_env, body)
    }

    /// Evaluate case expressions with pattern matching
    fn eval_case(
        &mut self,
        env: &Environment,
        scrutinee: &SpannedExpr,
        branches: &[(SpannedPattern, SpannedExpr)],
    ) -> RuntimeResult<Value> {
        let scrutinee_val = self.eval_expr(env, scrutinee)?;

        for (pattern, expr) in branches {
            if let Some(bindings) = self.match_pattern(pattern, &scrutinee_val)? {
                let new_env = env.extend();
                for (name, value) in bindings {
                    new_env.define(name, value);
                }
                return self.eval_expr(&new_env, expr);
            }
        }

        Err(RuntimeError::pattern_match_failure())
    }

    /// Evaluate if expressions
    fn eval_if(
        &mut self,
        env: &Environment,
        condition: &SpannedExpr,
        then_branch: &SpannedExpr,
        else_branch: &SpannedExpr,
    ) -> RuntimeResult<Value> {
        let cond_val = self.eval_expr(env, condition)?;
        if cond_val.is_truthy() {
            self.eval_expr(env, then_branch)
        } else {
            self.eval_expr(env, else_branch)
        }
    }

    /// Evaluate list literals
    fn eval_list(&mut self, env: &Environment, elements: &[SpannedExpr]) -> RuntimeResult<Value> {
        let mut values = Vec::new();
        for elem in elements {
            values.push(self.eval_expr(env, elem)?);
        }
        Ok(Value::List(values))
    }

    /// Evaluate cons expressions
    fn eval_cons(
        &mut self,
        env: &Environment,
        head: &SpannedExpr,
        tail: &SpannedExpr,
    ) -> RuntimeResult<Value> {
        let head_val = self.eval_expr(env, head)?;
        let tail_val = self.eval_expr(env, tail)?;

        match tail_val {
            Value::List(mut list) => {
                list.insert(0, head_val);
                Ok(Value::List(list))
            }
            _ => Err(RuntimeError::type_mismatch("list", tail_val.type_name())),
        }
    }

    /// Evaluate block expressions
    fn eval_block(&mut self, env: &Environment, exprs: &[SpannedExpr]) -> RuntimeResult<Value> {
        if exprs.is_empty() {
            return Ok(Value::Unit);
        }

        let block_env = env.extend();
        let mut result = Value::Unit;

        for (i, expr) in exprs.iter().enumerate() {
            result = self.eval_expr(&block_env, expr)?;

            // For let expressions that aren't the last in the block,
            // add their bindings to the block environment
            if i < exprs.len() - 1 {
                if let Expr::Let { name, value, .. } = &expr.node {
                    let val = self.eval_expr(&block_env, value)?;
                    block_env.define(name.clone(), val);
                }
            }
        }

        Ok(result)
    }

    /// Pattern matching implementation
    fn match_pattern(
        &self,
        pattern: &SpannedPattern,
        value: &Value,
    ) -> RuntimeResult<Option<Vec<(String, Value)>>> {
        match (&pattern.node, value) {
            (Pattern::Wild, _) => Ok(Some(vec![])),
            (Pattern::Var(name), val) => Ok(Some(vec![(name.clone(), val.clone())])),
            (Pattern::Lit(lit), val) => {
                let pattern_val = self.eval_literal(lit)?;
                if &pattern_val == val {
                    Ok(Some(vec![]))
                } else {
                    Ok(None)
                }
            }
            (Pattern::List(patterns), Value::List(values)) => {
                if patterns.len() != values.len() {
                    return Ok(None);
                }
                let mut bindings = Vec::new();
                for (pat, val) in patterns.iter().zip(values.iter()) {
                    match self.match_pattern(pat, val)? {
                        Some(mut pat_bindings) => bindings.append(&mut pat_bindings),
                        None => return Ok(None),
                    }
                }
                Ok(Some(bindings))
            }
            (Pattern::Cons { head, tail }, Value::List(values)) => {
                if values.is_empty() {
                    return Ok(None);
                }
                let head_val = &values[0];
                let tail_val = Value::List(values[1..].to_vec());

                let mut bindings = Vec::new();
                if let Some(mut head_bindings) = self.match_pattern(head, head_val)? {
                    bindings.append(&mut head_bindings);
                } else {
                    return Ok(None);
                }
                if let Some(mut tail_bindings) = self.match_pattern(tail, &tail_val)? {
                    bindings.append(&mut tail_bindings);
                } else {
                    return Ok(None);
                }
                Ok(Some(bindings))
            }
            (Pattern::Constructor { name, args: _ }, _) => {
                // For future ADT support
                Err(RuntimeError::custom(&format!(
                    "Constructor patterns not yet implemented: {}",
                    name
                )))
            }
            _ => Ok(None),
        }
    }

    /// Evaluate built-in functions
    fn eval_builtin(&self, name: &str, arg: Value) -> RuntimeResult<Value> {
        match name {
            "print" => {
                println!("{}", arg);
                Ok(Value::Unit)
            }
            "length" => match arg {
                Value::List(list) => Ok(Value::Int(list.len() as i64)),
                Value::String(s) => Ok(Value::Int(s.len() as i64)),
                _ => Err(RuntimeError::type_mismatch(
                    "list or string",
                    arg.type_name(),
                )),
            },
            "head" => match arg {
                Value::List(list) => {
                    if list.is_empty() {
                        Err(RuntimeError::empty_list("head"))
                    } else {
                        Ok(list[0].clone())
                    }
                }
                _ => Err(RuntimeError::type_mismatch("list", arg.type_name())),
            },
            "tail" => match arg {
                Value::List(list) => {
                    if list.is_empty() {
                        Err(RuntimeError::empty_list("tail"))
                    } else {
                        Ok(Value::List(list[1..].to_vec()))
                    }
                }
                _ => Err(RuntimeError::type_mismatch("list", arg.type_name())),
            },
            "empty?" => match arg {
                Value::List(list) => Ok(Value::Bool(list.is_empty())),
                _ => Err(RuntimeError::type_mismatch("list", arg.type_name())),
            },
            "reverse" => match arg {
                Value::List(mut list) => {
                    list.reverse();
                    Ok(Value::List(list))
                }
                _ => Err(RuntimeError::type_mismatch("list", arg.type_name())),
            },
            "abs" => match arg {
                Value::Int(n) => Ok(Value::Int(n.abs())),
                Value::Float(f) => Ok(Value::Float(f.abs())),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "int?" => Ok(Value::Bool(matches!(arg, Value::Int(_)))),
            "string?" => Ok(Value::Bool(matches!(arg, Value::String(_)))),
            "bool?" => Ok(Value::Bool(matches!(arg, Value::Bool(_)))),
            "list?" => Ok(Value::Bool(matches!(arg, Value::List(_)))),
            "function?" => Ok(Value::Bool(arg.is_callable())),
            _ => Err(RuntimeError::builtin_error(
                name,
                &format!("Unknown built-in function: {}", name),
            )),
        }
    }

    /// Get the current call stack (for debugging)
    pub fn call_stack(&self) -> &CallStack {
        &self.call_stack
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Span, Spanned};

    fn dummy_span() -> Span {
        Span::new(0, 0)
    }

    fn spanned<T>(node: T) -> Spanned<T> {
        Spanned::new(node, dummy_span())
    }

    #[test]
    fn test_eval_literal() {
        let mut eval = Evaluator::new();
        let env = Environment::new();

        let expr = spanned(Expr::Lit(Literal::Int(42)));
        assert_eq!(eval.eval(&env, &expr).unwrap(), Value::Int(42));

        let expr = spanned(Expr::Lit(Literal::String("hello".to_string())));
        assert_eq!(
            eval.eval(&env, &expr).unwrap(),
            Value::String("hello".to_string())
        );
    }

    #[test]
    fn test_eval_binop() {
        let mut eval = Evaluator::new();
        let env = Environment::new();

        let expr = spanned(Expr::BinOp {
            op: BinOp::Add,
            lhs: Box::new(spanned(Expr::Lit(Literal::Int(2)))),
            rhs: Box::new(spanned(Expr::Lit(Literal::Int(3)))),
        });

        assert_eq!(eval.eval(&env, &expr).unwrap(), Value::Int(5));
    }

    #[test]
    fn test_eval_let() {
        let mut eval = Evaluator::new();
        let env = Environment::new();

        let expr = spanned(Expr::Let {
            name: "x".to_string(),
            value: Box::new(spanned(Expr::Lit(Literal::Int(42)))),
            body: Box::new(spanned(Expr::Var("x".to_string()))),
        });

        assert_eq!(eval.eval(&env, &expr).unwrap(), Value::Int(42));
    }

    #[test]
    fn test_eval_lambda_and_application() {
        let mut eval = Evaluator::new();
        let env = Environment::new();

        // Create lambda: x |-> x + 1
        let lambda = spanned(Expr::Lambda {
            param: "x".to_string(),
            body: Box::new(spanned(Expr::BinOp {
                op: BinOp::Add,
                lhs: Box::new(spanned(Expr::Var("x".to_string()))),
                rhs: Box::new(spanned(Expr::Lit(Literal::Int(1)))),
            })),
        });

        // Apply lambda to 5
        let app = spanned(Expr::Apply {
            func: Box::new(lambda),
            arg: Box::new(spanned(Expr::Lit(Literal::Int(5)))),
        });

        assert_eq!(eval.eval(&env, &app).unwrap(), Value::Int(6));
    }
}
