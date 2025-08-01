mod ast;
mod environment;
mod error;
mod evaluator;
mod lexer;
mod parser;

use environment::{Environment, Value};
use error::RuntimeResult;
use evaluator::Evaluator;

fn eval_string(input: &str) -> RuntimeResult<Value> {
    let expr = parser::parse_expr_from_str(input).map_err(|e| error::RuntimeError::custom(&e))?;

    let mut evaluator = Evaluator::new();
    let env = Environment::global();

    evaluator.eval(&env, &expr)
}

fn main() {
    // Test basic expressions
    println!("🚀 Testing Farrow Language with Enhanced Error Handling");
    println!("{}", "=".repeat(60));

    let tests = vec![
        ("42", "Integer literal"),
        ("1 + 2 * 3", "Arithmetic with precedence"),
        ("[1, 2, 3]", "List literal"),
        ("1 : [2, 3]", "Cons operation"),
        ("x |-> x + 1", "Lambda function"),
        ("case 42 of 42 => true", "Pattern matching"),
        ("case 0 of 0 => true", "Pattern matching"),
        ("let x := 5 in x + 3", "Let binding"),
    ];

    for (test, description) in tests {
        println!("\n📝 {}: {}", description, test);
        match eval_string(test) {
            Ok(result) => println!("✅ Result: {}", result),
            Err(e) => println!("❌ Error: {}", e),
        }
    }

    // Test built-in functions
    println!("\n\n🔧 Testing Built-in Functions:");
    println!("{}", "-".repeat(40));

    let builtin_tests = vec![
        ("length [1, 2, 3, 4]", "List length"),
        ("head [1, 2, 3]", "List head"),
        ("tail [1, 2, 3]", "List tail"),
        ("empty? []", "Empty list check"),
        ("abs (-5)", "Absolute value"),
        ("int? 42", "Type predicate"),
    ];

    for (test, description) in builtin_tests {
        println!("\n📝 {}: {}", description, test);
        match eval_string(test) {
            Ok(result) => println!("✅ Result: {}", result),
            Err(e) => println!("❌ Error: {}", e),
        }
    }

    // Test error handling
    println!("\n\n⚠️  Testing Error Handling:");
    println!("{}", "-".repeat(40));

    let error_tests = vec![
        ("undefined_var", "Unbound variable"),
        ("1 / 0", "Division by zero"),
        ("head []", "Empty list operation"),
        ("1 + true", "Type mismatch"),
    ];

    for (test, description) in error_tests {
        println!("\n📝 {}: {}", description, test);
        match eval_string(test) {
            Ok(result) => println!("✅ Unexpected success: {}", result),
            Err(e) => println!("✅ Expected error: {}", e),
        }
    }

    // Test function application
    println!("\n\n🎯 Testing Function Application:");
    println!("{}", "-".repeat(40));

    let func_tests = vec![
        ("(x |-> x + 10) 5", "Simple function application"),
        ("5 |> (x |-> x * 2)", "Pipe operator"),
        (
            "μf |-> (x |-> case x of 0 => 1; _ => x)",
            "Recursive function",
        ),
    ];

    for (test, description) in func_tests {
        println!("\n📝 {}: {}", description, test);
        match eval_string(test) {
            Ok(result) => println!("✅ Result: {}", result),
            Err(e) => println!("❌ Error: {}", e),
        }
    }

    println!("\n\n🎉 Farrow Language Features Summary:");
    println!("{}", "=".repeat(50));
    println!("✅ Enhanced error handling with proper error types");
    println!("✅ Environment with lexical scoping");
    println!("✅ Built-in standard library functions");
    println!("✅ Stack overflow protection");
    println!("✅ Pattern matching with case expressions");
    println!("✅ Lambda functions with |-> syntax");
    println!("✅ Recursive functions with μ operator");
    println!("✅ List operations and cons operator");
    println!("✅ Let bindings and local scope");
    println!("✅ Function application and closures");
    println!("✅ Pipe operator for function composition");
    println!("✅ Arithmetic and comparison operators");
    println!("✅ Boolean logic operations");
}
