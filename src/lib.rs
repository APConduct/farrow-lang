//! Farrow Programming Language
//!
//! A purely functional programming language with pattern matching, recursion, and arrow-centric design.

pub mod ast;
pub mod environment;
pub mod error;
pub mod evaluator;
pub mod lexer;
pub mod parser;
pub mod repl;
pub mod reporting;

// Re-export commonly used types
pub use environment::{Environment, Value};
pub use error::{FarrowError, LexError, ParseError, RuntimeError, RuntimeResult};
pub use evaluator::Evaluator;
pub use parser::parse_expr_from_str;
pub use reporting::{print_error, print_info, print_success, print_warning};

/// Convenience function to evaluate a string expression
pub fn eval_string(input: &str) -> RuntimeResult<Value> {
    let expr = parse_expr_from_str(input).map_err(|e| RuntimeError::custom(&e))?;
    let mut evaluator = Evaluator::new();
    let env = Environment::global();
    evaluator.eval(&env, &expr)
}

/// Convenience function to evaluate a string and get the result as a string
pub fn eval_to_string(input: &str) -> Result<String, String> {
    eval_string(input)
        .map(|v| v.to_string())
        .map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_evaluation() {
        assert_eq!(eval_to_string("42").unwrap(), "42");
        assert_eq!(eval_to_string("1 + 2").unwrap(), "3");
        assert_eq!(eval_to_string("true").unwrap(), "true");
    }

    #[test]
    fn test_lambda() {
        assert_eq!(eval_to_string("(x |-> x + 1) 5").unwrap(), "6");
    }

    #[test]
    fn test_let_binding() {
        assert_eq!(eval_to_string("let x := 5 in x + 3").unwrap(), "8");
    }

    #[test]
    fn test_pattern_matching() {
        assert_eq!(
            eval_to_string("case 42 of 42 => true; _ => false").unwrap(),
            "true"
        );
    }

    #[test]
    fn test_error_handling() {
        assert!(eval_to_string("undefined_variable").is_err());
        assert!(eval_to_string("1 / 0").is_err());
    }
}
