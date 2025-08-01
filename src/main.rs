mod ast;
mod lexer;
mod parser;

use ast::{Expr, Literal, SpannedExpr};
use clap::{Arg, Command};
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result as RustylineResult};
use std::collections::HashMap;
use std::fs;

// Simple interpreter for testing
type Env = HashMap<String, Value>;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Unit,
    List(Vec<Value>),
    // TODO: Add function values, etc.
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Unit => write!(f, "()"),
            Value::List(values) => {
                write!(f, "[")?;
                for (i, val) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, "]")
            }
        }
    }
}

// Simple evaluator (placeholder for now)
fn eval_expr(env: &Env, expr: &SpannedExpr) -> std::result::Result<Value, String> {
    match &expr.node {
        Expr::Lit(lit) => match lit {
            Literal::Int(n) => Ok(Value::Int(*n)),
            Literal::Float(f) => Ok(Value::Float(*f)),
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Bool(b) => Ok(Value::Bool(*b)),
            Literal::Unit => Ok(Value::Unit),
        },
        Expr::Var(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| format!("Unbound variable: {}", name)),
        Expr::List(elements) => {
            let mut values = Vec::new();
            for elem in elements {
                values.push(eval_expr(env, elem)?);
            }
            Ok(Value::List(values))
        }
        Expr::BinOp { op, lhs, rhs } => {
            let left = eval_expr(env, lhs)?;
            let right = eval_expr(env, rhs)?;

            use ast::BinOp;
            match (op, &left, &right) {
                (BinOp::Add, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                (BinOp::Sub, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                (BinOp::Mul, Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                (BinOp::Div, Value::Int(a), Value::Int(b)) => {
                    if *b == 0 {
                        Err("Division by zero".to_string())
                    } else {
                        Ok(Value::Int(a / b))
                    }
                }
                (BinOp::Eq, a, b) => Ok(Value::Bool(a == b)),
                (BinOp::Neq, a, b) => Ok(Value::Bool(a != b)),
                (BinOp::Lt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                (BinOp::Gt, Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                _ => Err(format!(
                    "Unsupported operation: {} {} {}",
                    format_value(&left),
                    op,
                    format_value(&right)
                )),
            }
        }
        Expr::Let { name, value, body } => {
            let val = eval_expr(env, value)?;
            let mut new_env = env.clone();
            new_env.insert(name.clone(), val);
            eval_expr(&new_env, body)
        }
        _ => Err(format!(
            "Evaluation not yet implemented for: {:?}",
            expr.node
        )),
    }
}

fn format_value(value: &Value) -> String {
    match value {
        Value::Int(n) => n.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => format!("\"{}\"", s),
        Value::Bool(b) => b.to_string(),
        Value::Unit => "()".to_string(),
        Value::List(_) => format!("{}", value),
    }
}

fn eval_program(exprs: Vec<SpannedExpr>) -> std::result::Result<Value, String> {
    if exprs.is_empty() {
        return Ok(Value::Unit);
    }

    let mut env = HashMap::new();
    let mut result = Value::Unit;

    for expr in exprs {
        result = eval_expr(&env, &expr)?;

        // If it's a let binding at top level, add to environment
        if let Expr::Let { name, value, .. } = &expr.node {
            let val = eval_expr(&env, value)?;
            env.insert(name.clone(), val);
        }
    }

    Ok(result)
}

fn eval_string(input: &str) -> std::result::Result<Value, String> {
    match parser::parse_expr_from_str(input) {
        Ok(expr) => eval_expr(&HashMap::new(), &expr),
        Err(e) => Err(e),
    }
}

fn eval_file(file_path: &str) -> std::result::Result<(), String> {
    let contents = fs::read_to_string(file_path)
        .map_err(|e| format!("Error reading file {}: {}", file_path, e))?;

    println!("File contents:\n{}", contents);

    match parser::parse_program_from_str(&contents) {
        Ok(exprs) => {
            println!("Parsed expressions: {:#?}", exprs);
            match eval_program(exprs) {
                Ok(result) => {
                    println!("Result: {}", result);
                    Ok(())
                }
                Err(e) => Err(format!("Evaluation error: {}", e)),
            }
        }
        Err(e) => Err(format!("Parse error: {}", e)),
    }
}

fn repl() -> RustylineResult<()> {
    let mut rl = DefaultEditor::new()?;

    println!("F↦ v0.1 (Rust with Logos/Chumsky) - Ctrl-D to quit, :l <file> to load a file");

    loop {
        let readline = rl.readline("F↦> ");
        match readline {
            Ok(line) => {
                let line = line.trim();

                if line.is_empty() {
                    continue;
                }

                if line == ":q" {
                    break;
                }

                if line.starts_with(":l ") {
                    let file_path = &line[3..];
                    match eval_file(file_path) {
                        Ok(()) => {}
                        Err(e) => println!("Error: {}", e),
                    }
                    continue;
                }

                if line.starts_with(":t ") {
                    // Show tokens for debugging
                    let input = &line[3..];
                    match lexer::tokenize(input) {
                        Ok(tokens) => {
                            println!("Tokens: {:?}", tokens);
                        }
                        Err(e) => println!("Tokenization error: {:?}", e),
                    }
                    continue;
                }

                if line.starts_with(":p ") {
                    // Show parse tree for debugging
                    let input = &line[3..];
                    match parser::parse_expr_from_str(input) {
                        Ok(expr) => {
                            println!("Parse tree: {:#?}", expr);
                        }
                        Err(e) => println!("Parse error: {}", e),
                    }
                    continue;
                }

                match eval_string(line) {
                    Ok(result) => println!("{}", result),
                    Err(e) => println!("Error: {}", e),
                }

                rl.add_history_entry(line)?;
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}

fn main() {
    let matches = Command::new("farrow")
        .version("0.1.0")
        .about("F↦ programming language interpreter")
        .arg(
            Arg::new("file")
                .help("File to execute")
                .value_name("FILE")
                .index(1),
        )
        .get_matches();

    if let Some(file_path) = matches.get_one::<String>("file") {
        if let Err(e) = eval_file(file_path) {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    } else {
        if let Err(e) = repl() {
            eprintln!("REPL error: {}", e);
            std::process::exit(1);
        }
    }
}
