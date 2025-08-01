use crate::environment::Environment;
use crate::evaluator::Evaluator;
use crate::parser::parse_expr_from_str;
use crate::reporting::{print_error, print_success};
use std::io::{self, Write};

pub struct Repl {
    evaluator: Evaluator,
    environment: Environment,
    history: Vec<String>,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            evaluator: Evaluator::new(),
            environment: Environment::global(),
            history: Vec::new(),
        }
    }

    pub fn run(&mut self) {
        println!("🚀 Welcome to Farrow REPL!");
        println!("Type expressions to evaluate them, :help for commands, :quit to exit.");
        println!("{}", "=".repeat(60));

        loop {
            print!("farrow> ");
            io::stdout().flush().unwrap();

            let mut input = String::new();
            match io::stdin().read_line(&mut input) {
                Ok(_) => {
                    let input = input.trim();

                    if input.is_empty() {
                        continue;
                    }

                    // Handle REPL commands
                    if input.starts_with(':') {
                        if self.handle_command(input) {
                            break; // Exit REPL
                        }
                        continue;
                    }

                    // Store in history
                    self.history.push(input.to_string());

                    // Evaluate expression
                    self.eval_and_print(input);
                }
                Err(error) => {
                    eprintln!("Error reading input: {}", error);
                    break;
                }
            }
        }

        println!("Goodbye! 👋");
    }

    fn handle_command(&mut self, command: &str) -> bool {
        match command {
            ":quit" | ":q" | ":exit" => {
                return true;
            }
            ":help" | ":h" => {
                self.show_help();
            }
            ":clear" => {
                print!("\x1B[2J\x1B[H"); // Clear screen
                io::stdout().flush().unwrap();
            }
            ":history" => {
                self.show_history();
            }
            ":env" => {
                self.show_environment();
            }
            ":reset" => {
                self.evaluator = Evaluator::new();
                self.environment = Environment::global();
                print_success("Environment reset");
            }
            _ => {
                println!("Unknown command: {}", command);
                println!("Type :help for available commands");
            }
        }
        false
    }

    fn eval_and_print(&mut self, input: &str) {
        // Try to parse as an assignment first (x := value)
        if let Some((name, expr_str)) = self.parse_assignment(input) {
            match parse_expr_from_str(expr_str) {
                Ok(expr) => match self.evaluator.eval(&self.environment, &expr) {
                    Ok(value) => {
                        self.environment.define(name.clone(), value.clone());
                        println!("{} := {}", name, value);
                    }
                    Err(error) => {
                        print_error(&error.into(), "repl", input);
                    }
                },
                Err(parse_error) => {
                    println!("Parse error: {}", parse_error);
                }
            }
        } else {
            // Regular expression evaluation
            match parse_expr_from_str(input) {
                Ok(expr) => match self.evaluator.eval(&self.environment, &expr) {
                    Ok(value) => {
                        println!("{}", value);
                    }
                    Err(error) => {
                        print_error(&error.into(), "repl", input);
                    }
                },
                Err(parse_error) => {
                    println!("Parse error: {}", parse_error);
                }
            }
        }
    }

    fn parse_assignment<'a>(&self, input: &'a str) -> Option<(String, &'a str)> {
        if let Some(pos) = input.find(":=") {
            let name = input[..pos].trim();
            let expr = input[pos + 2..].trim();

            // Simple validation: name should be a valid identifier
            if name.chars().all(|c| c.is_alphanumeric() || c == '_')
                && name
                    .chars()
                    .next()
                    .map_or(false, |c| c.is_alphabetic() || c == '_')
            {
                Some((name.to_string(), expr))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn show_help(&self) {
        println!("Farrow REPL Commands:");
        println!("  :help, :h       - Show this help message");
        println!("  :quit, :q       - Exit the REPL");
        println!("  :clear          - Clear the screen");
        println!("  :history        - Show command history");
        println!("  :env            - Show current environment");
        println!("  :reset          - Reset environment to initial state");
        println!();
        println!("Language Features:");
        println!("  42              - Integer literals");
        println!("  \"hello\"         - String literals");
        println!("  true, false     - Boolean literals");
        println!("  [1, 2, 3]       - List literals");
        println!("  1 : [2, 3]      - Cons operator");
        println!("  x |-> x + 1     - Lambda functions");
        println!("  let x := 5 in x - Let bindings");
        println!("  case x of ...   - Pattern matching");
        println!("  μf |-> ...      - Recursive functions");
        println!("  x := 42         - Variable assignment (REPL only)");
        println!();
        println!("Built-in Functions:");
        println!("  length, head, tail, empty?, abs, int?, string?, bool?, list?");
    }

    fn show_history(&self) {
        if self.history.is_empty() {
            println!("No history available");
        } else {
            println!("Command History:");
            for (i, cmd) in self.history.iter().enumerate() {
                println!("  {}: {}", i + 1, cmd);
            }
        }
    }

    fn show_environment(&self) {
        println!("Current Environment:");
        let bindings = self.environment.local_bindings();
        if bindings.is_empty() {
            println!("  (no user-defined variables)");
        } else {
            for (name, value) in bindings {
                println!("  {} = {}", name, value);
            }
        }

        // Show some built-ins
        println!("Built-in Functions:");
        let builtins = vec![
            "length",
            "head",
            "tail",
            "empty?",
            "abs",
            "int?",
            "string?",
            "bool?",
            "list?",
            "function?",
        ];
        for builtin in builtins {
            println!("  {}", builtin);
        }
    }
}

impl Default for Repl {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_assignment() {
        let repl = Repl::new();

        assert_eq!(
            repl.parse_assignment("x := 42"),
            Some(("x".to_string(), "42"))
        );
        assert_eq!(
            repl.parse_assignment("foo := 1 + 2"),
            Some(("foo".to_string(), "1 + 2"))
        );
        assert_eq!(repl.parse_assignment("invalid name := 42"), None);
        assert_eq!(repl.parse_assignment("42 := x"), None);
        assert_eq!(repl.parse_assignment("just an expression"), None);
    }

    #[test]
    fn test_repl_creation() {
        let repl = Repl::new();
        assert!(repl.history.is_empty());
    }
}
