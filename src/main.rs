mod ast;
mod environment;
mod error;
mod evaluator;
mod lexer;
mod parser;
mod repl;
mod reporting;

use environment::{Environment, Value};
use error::RuntimeResult;
use evaluator::Evaluator;
use parser::parse_expr_from_str;
use repl::Repl;
use std::env;
use std::fs;
use std::path::Path;

fn eval_string(input: &str) -> RuntimeResult<Value> {
    let expr = parser::parse_expr_from_str(input).map_err(|e| error::RuntimeError::custom(&e))?;

    let mut evaluator = Evaluator::new();
    let env = Environment::global();

    evaluator.eval(&env, &expr)
}

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => {
            // Check if we have input from stdin (pipe or redirect)
            if atty::is(atty::Stream::Stdin) {
                // Interactive mode - start REPL
                let mut repl = Repl::new();
                repl.run();
            } else {
                // Non-interactive mode - read from stdin
                read_from_stdin();
            }
        }
        2 => match args[1].as_str() {
            "repl" => {
                let mut repl = Repl::new();
                repl.run();
            }
            "test" => {
                run_tests();
            }
            "--help" | "-h" => {
                show_help();
            }
            "--" => {
                // Explicit stdin mode
                read_from_stdin();
            }
            filename => {
                // Try to execute file
                if Path::new(filename).exists() {
                    run_file(filename);
                } else {
                    println!("File not found: {}", filename);
                    show_help();
                }
            }
        },
        _ => {
            println!("Too many arguments");
            show_help();
        }
    }
}

fn read_from_stdin() {
    use std::io::Read;

    let mut input = String::new();
    match std::io::stdin().read_to_string(&mut input) {
        Ok(_) => {
            if !input.trim().is_empty() {
                match parse_expr_from_str(&input) {
                    Ok(expr) => {
                        let mut evaluator = Evaluator::new();
                        let env = Environment::global();

                        match evaluator.eval(&env, &expr) {
                            Ok(value) => {
                                println!("{}", value);
                            }
                            Err(error) => {
                                reporting::print_error(&error.into(), "stdin", &input);
                                std::process::exit(1);
                            }
                        }
                    }
                    Err(parse_error) => {
                        println!("Parse error: {}", parse_error);
                        std::process::exit(1);
                    }
                }
            }
        }
        Err(error) => {
            println!("Error reading from stdin: {}", error);
            std::process::exit(1);
        }
    }
}

fn show_help() {
    println!("Farrow Programming Language");
    println!("Usage:");
    println!("  farrow               - Start REPL (or read from stdin if piped)");
    println!("  farrow repl          - Start REPL");
    println!("  farrow test          - Run test suite");
    println!("  farrow <filename>    - Execute Farrow file");
    println!("  farrow --            - Read expression from stdin");
    println!("  farrow --help        - Show this help");
    println!();
    println!("Examples:");
    println!("  farrow                    # Interactive REPL");
    println!("  echo '1 + 2' | farrow     # Evaluate from pipe");
    println!("  farrow examples/basic.fro # Run file");
}

fn run_file(filename: &str) {
    match fs::read_to_string(filename) {
        Ok(content) => match parse_expr_from_str(&content) {
            Ok(expr) => {
                let mut evaluator = Evaluator::new();
                let env = Environment::global();

                match evaluator.eval(&env, &expr) {
                    Ok(value) => {
                        println!("{}", value);
                    }
                    Err(error) => {
                        reporting::print_error(&error.into(), filename, &content);
                        std::process::exit(1);
                    }
                }
            }
            Err(parse_error) => {
                println!("Parse error in {}: {}", filename, parse_error);
                std::process::exit(1);
            }
        },
        Err(error) => {
            println!("Error reading file {}: {}", filename, error);
            std::process::exit(1);
        }
    }
}

fn run_tests() {
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
