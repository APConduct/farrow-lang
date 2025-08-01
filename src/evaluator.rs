use crate::ast::{BinOp, Expr, Literal, Pattern, SpannedExpr, SpannedPattern, UnaryOp};
use crate::environment::{CallFrame, CallStack, Environment, Value};
use crate::error::{RuntimeError, RuntimeResult};
use std::collections::HashMap;

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
            Value::BuiltinFunction2(name, first_arg) => {
                self.eval_builtin_2(&name, &first_arg, arg_val)
            }
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
                    Value::BuiltinFunction2(name, first_arg) => {
                        self.eval_builtin_2(&name, &first_arg, left_val)
                    }
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

        for expr in exprs.iter() {
            // Special handling for let statements in blocks
            if let Expr::Let { name, value, body } = &expr.node {
                // Check if this is a block-style let (with unit body)
                if matches!(body.node, Expr::Lit(Literal::Unit)) {
                    // Evaluate the value and add to block environment
                    let val = self.eval_expr(&block_env, value)?;
                    block_env.define(name.clone(), val);
                    result = Value::Unit;
                } else {
                    // Regular let expression
                    result = self.eval_expr(&block_env, expr)?;
                }
            } else {
                // Regular expression
                result = self.eval_expr(&block_env, expr)?;
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
            (
                Pattern::Constructor { name, args },
                Value::Constructor {
                    constructor,
                    fields,
                },
            ) => {
                // Match constructor name
                if name != constructor {
                    return Ok(None);
                }

                // Match arity
                if args.len() != fields.len() {
                    return Ok(None);
                }

                // Match all arguments
                let mut bindings = Vec::new();
                for (pat, val) in args.iter().zip(fields.iter()) {
                    match self.match_pattern(pat, val)? {
                        Some(mut pat_bindings) => bindings.append(&mut pat_bindings),
                        None => return Ok(None),
                    }
                }
                Ok(Some(bindings))
            }
            (Pattern::Constructor { name, args }, _) => {
                // Constructor pattern doesn't match non-constructor value
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    /// Evaluate built-in functions
    fn eval_builtin(&mut self, name: &str, arg: Value) -> RuntimeResult<Value> {
        // Check if this is a multi-argument function that needs currying
        match name {
            "map" | "filter" | "fold" | "reduce" | "zip" | "take" | "drop" | "concat"
            | "contains" | "index_of" | "string_concat" | "string_split" | "string_join"
            | "string_replace" | "string_contains" | "string_starts_with" | "string_ends_with"
            | "char_at" | "min" | "max" | "pow" | "const" | "compose" | "apply" => {
                // Return a partially applied function
                return Ok(Value::BuiltinFunction2(name.to_string(), Box::new(arg)));
            }
            _ => {}
        }
        match name {
            // I/O functions
            "print" => {
                print!("{}", arg);
                Ok(Value::Unit)
            }
            "println" | "print_line" => {
                println!("{}", arg);
                Ok(Value::Unit)
            }

            // Basic list functions
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
                Value::String(s) => Ok(Value::Bool(s.is_empty())),
                _ => Err(RuntimeError::type_mismatch(
                    "list or string",
                    arg.type_name(),
                )),
            },
            "reverse" => match arg {
                Value::List(mut list) => {
                    list.reverse();
                    Ok(Value::List(list))
                }
                Value::String(s) => Ok(Value::String(s.chars().rev().collect())),
                _ => Err(RuntimeError::type_mismatch(
                    "list or string",
                    arg.type_name(),
                )),
            },

            // Advanced list functions (higher-order - need special handling)
            "sort" => match arg {
                Value::List(mut list) => {
                    // Simple sort for comparable values
                    list.sort_by(|a, b| match (a, b) {
                        (Value::Int(a), Value::Int(b)) => a.cmp(b),
                        (Value::Float(a), Value::Float(b)) => {
                            a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
                        }
                        (Value::String(a), Value::String(b)) => a.cmp(b),
                        _ => std::cmp::Ordering::Equal,
                    });
                    Ok(Value::List(list))
                }
                _ => Err(RuntimeError::type_mismatch("list", arg.type_name())),
            },
            "flatten" => match arg {
                Value::List(list) => {
                    let mut result = Vec::new();
                    for item in list {
                        match item {
                            Value::List(inner) => result.extend(inner),
                            other => result.push(other),
                        }
                    }
                    Ok(Value::List(result))
                }
                _ => Err(RuntimeError::type_mismatch("list", arg.type_name())),
            },

            // String functions
            "string_length" => match arg {
                Value::String(s) => Ok(Value::Int(s.len() as i64)),
                _ => Err(RuntimeError::type_mismatch("string", arg.type_name())),
            },
            "string_trim" => match arg {
                Value::String(s) => Ok(Value::String(s.trim().to_string())),
                _ => Err(RuntimeError::type_mismatch("string", arg.type_name())),
            },
            "string_upper" => match arg {
                Value::String(s) => Ok(Value::String(s.to_uppercase())),
                _ => Err(RuntimeError::type_mismatch("string", arg.type_name())),
            },
            "string_lower" => match arg {
                Value::String(s) => Ok(Value::String(s.to_lowercase())),
                _ => Err(RuntimeError::type_mismatch("string", arg.type_name())),
            },

            // Basic math functions
            "abs" => match arg {
                Value::Int(n) => Ok(Value::Int(n.abs())),
                Value::Float(f) => Ok(Value::Float(f.abs())),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "sign" => match arg {
                Value::Int(n) => Ok(Value::Int(n.signum())),
                Value::Float(f) => Ok(Value::Float(f.signum())),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "sqrt" => match arg {
                Value::Int(n) => Ok(Value::Float((n as f64).sqrt())),
                Value::Float(f) => Ok(Value::Float(f.sqrt())),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "exp" => match arg {
                Value::Int(n) => Ok(Value::Float((n as f64).exp())),
                Value::Float(f) => Ok(Value::Float(f.exp())),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "log" => match arg {
                Value::Int(n) => Ok(Value::Float((n as f64).log10())),
                Value::Float(f) => Ok(Value::Float(f.log10())),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "ln" => match arg {
                Value::Int(n) => Ok(Value::Float((n as f64).ln())),
                Value::Float(f) => Ok(Value::Float(f.ln())),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "sin" => match arg {
                Value::Int(n) => Ok(Value::Float((n as f64).sin())),
                Value::Float(f) => Ok(Value::Float(f.sin())),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "cos" => match arg {
                Value::Int(n) => Ok(Value::Float((n as f64).cos())),
                Value::Float(f) => Ok(Value::Float(f.cos())),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "tan" => match arg {
                Value::Int(n) => Ok(Value::Float((n as f64).tan())),
                Value::Float(f) => Ok(Value::Float(f.tan())),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "asin" => match arg {
                Value::Int(n) => Ok(Value::Float((n as f64).asin())),
                Value::Float(f) => Ok(Value::Float(f.asin())),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "acos" => match arg {
                Value::Int(n) => Ok(Value::Float((n as f64).acos())),
                Value::Float(f) => Ok(Value::Float(f.acos())),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "atan" => match arg {
                Value::Int(n) => Ok(Value::Float((n as f64).atan())),
                Value::Float(f) => Ok(Value::Float(f.atan())),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "floor" => match arg {
                Value::Int(n) => Ok(Value::Int(n)),
                Value::Float(f) => Ok(Value::Int(f.floor() as i64)),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "ceil" => match arg {
                Value::Int(n) => Ok(Value::Int(n)),
                Value::Float(f) => Ok(Value::Int(f.ceil() as i64)),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },
            "round" => match arg {
                Value::Int(n) => Ok(Value::Int(n)),
                Value::Float(f) => Ok(Value::Int(f.round() as i64)),
                _ => Err(RuntimeError::type_mismatch("number", arg.type_name())),
            },

            // Type conversion functions
            "to_string" => match arg {
                Value::String(s) => Ok(Value::String(s)),
                other => Ok(Value::String(other.to_string())),
            },
            "to_int" => match arg {
                Value::Int(n) => Ok(Value::Int(n)),
                Value::Float(f) => Ok(Value::Int(f as i64)),
                Value::String(s) => s
                    .parse::<i64>()
                    .map(Value::Int)
                    .map_err(|_| RuntimeError::custom(&format!("Cannot parse '{}' as integer", s))),
                _ => Err(RuntimeError::type_mismatch(
                    "number or string",
                    arg.type_name(),
                )),
            },
            "to_float" => match arg {
                Value::Int(n) => Ok(Value::Float(n as f64)),
                Value::Float(f) => Ok(Value::Float(f)),
                Value::String(s) => s
                    .parse::<f64>()
                    .map(Value::Float)
                    .map_err(|_| RuntimeError::custom(&format!("Cannot parse '{}' as float", s))),
                _ => Err(RuntimeError::type_mismatch(
                    "number or string",
                    arg.type_name(),
                )),
            },

            // Type predicates
            "int?" => Ok(Value::Bool(matches!(arg, Value::Int(_)))),
            "float?" => Ok(Value::Bool(matches!(arg, Value::Float(_)))),
            "string?" => Ok(Value::Bool(matches!(arg, Value::String(_)))),
            "bool?" => Ok(Value::Bool(matches!(arg, Value::Bool(_)))),
            "list?" => Ok(Value::Bool(matches!(arg, Value::List(_)))),
            "function?" => Ok(Value::Bool(arg.is_callable())),
            "unit?" => Ok(Value::Bool(matches!(arg, Value::Unit))),

            // Utility functions
            "identity" => Ok(arg),

            // ADT constructor functions
            name if self.is_constructor_name(name) => {
                // Single-argument constructor
                Ok(Value::Constructor {
                    constructor: name.to_string(),
                    fields: vec![arg],
                })
            }

            _ => Err(RuntimeError::builtin_error(
                name,
                &format!("Unknown built-in function: {}", name),
            )),
        }
    }

    /// Evaluate two-argument built-in functions
    fn eval_builtin_2(&mut self, name: &str, arg1: &Value, arg2: Value) -> RuntimeResult<Value> {
        match name {
            "map" => match (arg1, arg2) {
                (func, Value::List(list)) => {
                    let mut result = Vec::new();
                    for item in list {
                        let mapped = match func {
                            Value::Function { param, body, env } => {
                                let new_env = env.extend();
                                new_env.define(param.clone(), item);
                                self.eval_expr(&new_env, body)?
                            }
                            Value::BuiltinFunction(name) => self.eval_builtin(name, item)?,
                            _ => return Err(RuntimeError::invalid_application(func.type_name())),
                        };
                        result.push(mapped);
                    }
                    Ok(Value::List(result))
                }
                _ => Err(RuntimeError::type_mismatch("function and list", "other")),
            },
            "filter" => match (arg1, arg2) {
                (func, Value::List(list)) => {
                    let mut result = Vec::new();
                    for item in list {
                        let keep = match func {
                            Value::Function { param, body, env } => {
                                let new_env = env.extend();
                                new_env.define(param.clone(), item.clone());
                                self.eval_expr(&new_env, body)?
                            }
                            Value::BuiltinFunction(name) => {
                                self.eval_builtin(name, item.clone())?
                            }
                            _ => return Err(RuntimeError::invalid_application(func.type_name())),
                        };
                        if keep.is_truthy() {
                            result.push(item);
                        }
                    }
                    Ok(Value::List(result))
                }
                _ => Err(RuntimeError::type_mismatch("function and list", "other")),
            },
            "min" => match (arg1, &arg2) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.min(b))),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.min(*b))),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).min(*b))),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.min(*b as f64))),
                _ => Err(RuntimeError::type_mismatch("two numbers", "other")),
            },
            "max" => match (arg1, &arg2) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.max(b))),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.max(*b))),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).max(*b))),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.max(*b as f64))),
                _ => Err(RuntimeError::type_mismatch("two numbers", "other")),
            },
            "pow" => match (arg1, &arg2) {
                (Value::Int(base), Value::Int(exp)) => {
                    Ok(Value::Float((*base as f64).powf(*exp as f64)))
                }
                (Value::Float(base), Value::Float(exp)) => Ok(Value::Float(base.powf(*exp))),
                (Value::Int(base), Value::Float(exp)) => {
                    Ok(Value::Float((*base as f64).powf(*exp)))
                }
                (Value::Float(base), Value::Int(exp)) => Ok(Value::Float(base.powf(*exp as f64))),
                _ => Err(RuntimeError::type_mismatch("two numbers", "other")),
            },
            "take" => match (arg1, arg2) {
                (Value::Int(n), Value::List(list)) => {
                    let take_count = (*n as usize).min(list.len());
                    Ok(Value::List(list.into_iter().take(take_count).collect()))
                }
                _ => Err(RuntimeError::type_mismatch("integer and list", "other")),
            },
            "drop" => match (arg1, arg2) {
                (Value::Int(n), Value::List(list)) => {
                    let drop_count = (*n as usize).min(list.len());
                    Ok(Value::List(list.into_iter().skip(drop_count).collect()))
                }
                _ => Err(RuntimeError::type_mismatch("integer and list", "other")),
            },
            "concat" => match (arg1, arg2) {
                (Value::List(list1), Value::List(list2)) => {
                    let mut result = list1.clone();
                    result.extend(list2);
                    Ok(Value::List(result))
                }
                _ => Err(RuntimeError::type_mismatch("two lists", "other")),
            },
            "contains" => match (arg1, arg2) {
                (needle, Value::List(haystack)) => Ok(Value::Bool(haystack.contains(needle))),
                (Value::String(needle), Value::String(haystack)) => {
                    Ok(Value::Bool(haystack.contains(needle)))
                }
                _ => Err(RuntimeError::type_mismatch(
                    "value and list/string",
                    "other",
                )),
            },
            "string_concat" => match (arg1, arg2) {
                (Value::String(s1), Value::String(s2)) => {
                    Ok(Value::String(format!("{}{}", s1, s2)))
                }
                _ => Err(RuntimeError::type_mismatch("two strings", "other")),
            },
            "string_split" => match (arg1, arg2) {
                (Value::String(delimiter), Value::String(text)) => {
                    let parts: Vec<Value> = text
                        .split(delimiter)
                        .map(|s| Value::String(s.to_string()))
                        .collect();
                    Ok(Value::List(parts))
                }
                _ => Err(RuntimeError::type_mismatch("two strings", "other")),
            },
            "string_contains" => match (arg1, arg2) {
                (Value::String(needle), Value::String(haystack)) => {
                    Ok(Value::Bool(haystack.contains(needle)))
                }
                _ => Err(RuntimeError::type_mismatch("two strings", "other")),
            },
            "string_starts_with" => match (arg1, arg2) {
                (Value::String(prefix), Value::String(text)) => {
                    Ok(Value::Bool(text.starts_with(prefix)))
                }
                _ => Err(RuntimeError::type_mismatch("two strings", "other")),
            },
            "string_ends_with" => match (arg1, arg2) {
                (Value::String(suffix), Value::String(text)) => {
                    Ok(Value::Bool(text.ends_with(suffix)))
                }
                _ => Err(RuntimeError::type_mismatch("two strings", "other")),
            },
            "char_at" => match (arg1, arg2) {
                (Value::Int(index), Value::String(text)) => {
                    if *index < 0 || *index >= text.len() as i64 {
                        Err(RuntimeError::custom("String index out of bounds"))
                    } else {
                        let char = text.chars().nth(*index as usize).unwrap();
                        Ok(Value::String(char.to_string()))
                    }
                }
                _ => Err(RuntimeError::type_mismatch("integer and string", "other")),
            },
            "const" => {
                // const x y = x (returns first argument, ignoring second)
                Ok(arg1.clone())
            }
            "apply" => {
                // apply f x = f x
                match arg1 {
                    Value::Function { param, body, env } => {
                        let new_env = env.extend();
                        new_env.define(param.clone(), arg2);
                        self.eval_expr(&new_env, body)
                    }
                    Value::BuiltinFunction(name) => self.eval_builtin(name, arg2),
                    _ => Err(RuntimeError::invalid_application(arg1.type_name())),
                }
            }
            _ => {
                // Check if this is a multi-argument constructor
                if self.is_constructor_name(name) {
                    Ok(Value::Constructor {
                        constructor: name.to_string(),
                        fields: vec![arg1.clone(), arg2],
                    })
                } else {
                    Err(RuntimeError::builtin_error(
                        name,
                        &format!("Unknown two-argument built-in function: {}", name),
                    ))
                }
            }
        }
    }

    /// Get the current call stack (for debugging)
    pub fn call_stack(&self) -> &CallStack {
        &self.call_stack
    }

    /// Check if a name is a constructor (starts with uppercase)
    fn is_constructor_name(&self, name: &str) -> bool {
        name.chars()
            .next()
            .map_or(false, |c| c.is_ascii_uppercase())
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
