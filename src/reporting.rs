use crate::error::{FarrowError, ParseError, RuntimeError};
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};

/// Create a beautiful error report using Ariadne
pub fn create_error_report<'a>(
    error: &'a FarrowError,
    source_name: &'a str,
    source_code: &'a str,
) -> Report<'a, (String, std::ops::Range<usize>)> {
    match error {
        FarrowError::Runtime(runtime_error) => {
            create_runtime_error_report(runtime_error, source_name, source_code)
        }
        FarrowError::Parse(parse_error) => {
            create_parse_error_report(parse_error, source_name, source_code)
        }
        FarrowError::Lex(lex_error) => create_lex_error_report(lex_error, source_name, source_code),
    }
}

fn create_runtime_error_report<'a>(
    error: &'a RuntimeError,
    source_name: &'a str,
    source_code: &'a str,
) -> Report<'a, (String, std::ops::Range<usize>)> {
    let mut report = Report::build(ReportKind::Error, source_name.to_string(), 0)
        .with_message(format!("Runtime Error: {}", error));

    if let Some(span) = error.span() {
        let range = span.start..span.end;

        match error {
            RuntimeError::UnboundVariable { name, .. } => {
                report = report.with_label(
                    Label::new((source_name.to_string(), range))
                        .with_message(format!("Variable '{}' is not defined", name.fg(Color::Red)))
                        .with_color(Color::Red),
                );
            }
            RuntimeError::TypeMismatch {
                expected, found, ..
            } => {
                report = report.with_label(
                    Label::new((source_name.to_string(), range))
                        .with_message(format!(
                            "Expected {} but found {}",
                            expected.fg(Color::Green),
                            found.fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                );
            }
            RuntimeError::DivisionByZero { .. } => {
                report = report.with_label(
                    Label::new((source_name.to_string(), range))
                        .with_message("Division by zero is not allowed".fg(Color::Red))
                        .with_color(Color::Red),
                );
            }
            RuntimeError::PatternMatchFailure { .. } => {
                report = report.with_label(
                    Label::new((source_name.to_string(), range))
                        .with_message("No pattern matched the given value".fg(Color::Red))
                        .with_color(Color::Red),
                );
            }
            RuntimeError::InvalidApplication { value_type, .. } => {
                report = report.with_label(
                    Label::new((source_name.to_string(), range))
                        .with_message(format!(
                            "Cannot call {} as a function",
                            value_type.fg(Color::Red)
                        ))
                        .with_color(Color::Red),
                );
            }
            RuntimeError::EmptyList { operation, .. } => {
                report = report.with_label(
                    Label::new((source_name.to_string(), range))
                        .with_message(format!(
                            "Cannot perform '{}' on an empty list",
                            operation.fg(Color::Yellow)
                        ))
                        .with_color(Color::Red),
                );
            }
            RuntimeError::StackOverflow { .. } => {
                report = report.with_label(
                    Label::new((source_name.to_string(), range))
                        .with_message("Stack overflow: recursion too deep".fg(Color::Red))
                        .with_color(Color::Red),
                );
            }
            _ => {
                report = report.with_label(
                    Label::new((source_name.to_string(), range))
                        .with_message(format!("{}", error).fg(Color::Red))
                        .with_color(Color::Red),
                );
            }
        }
    }

    report.finish()
}

fn create_parse_error_report<'a>(
    error: &'a ParseError,
    source_name: &'a str,
    _source_code: &'a str,
) -> Report<'a, (String, std::ops::Range<usize>)> {
    let span = error.span();
    let range = span.start..span.end;

    let mut report = Report::build(ReportKind::Error, source_name.to_string(), span.start)
        .with_message(format!("Parse Error: {}", error));

    match error {
        ParseError::UnexpectedToken {
            expected, found, ..
        } => {
            report = report.with_label(
                Label::new((source_name.to_string(), range))
                    .with_message(format!(
                        "Expected {} but found {}",
                        expected.fg(Color::Green),
                        found.fg(Color::Red)
                    ))
                    .with_color(Color::Red),
            );
        }
        ParseError::UnexpectedEof { expected, .. } => {
            report = report.with_label(
                Label::new((source_name.to_string(), range))
                    .with_message(format!(
                        "Unexpected end of input, expected {}",
                        expected.fg(Color::Green)
                    ))
                    .with_color(Color::Red),
            );
        }
        ParseError::InvalidSyntax { message, .. } => {
            report = report.with_label(
                Label::new((source_name.to_string(), range))
                    .with_message(message.fg(Color::Red))
                    .with_color(Color::Red),
            );
        }
        ParseError::LexError { message, .. } => {
            report = report.with_label(
                Label::new((source_name.to_string(), range))
                    .with_message(format!("Lexer error: {}", message.fg(Color::Red)))
                    .with_color(Color::Red),
            );
        }
    }

    report.finish()
}

fn create_lex_error_report<'a>(
    error: &'a crate::error::LexError,
    source_name: &'a str,
    _source_code: &'a str,
) -> Report<'a, (String, std::ops::Range<usize>)> {
    let (position, message) = match error {
        crate::error::LexError::InvalidCharacter {
            character,
            position,
        } => (*position, format!("Invalid character '{}'", character)),
        crate::error::LexError::UnterminatedString { start } => {
            (*start, "Unterminated string literal".to_string())
        }
        crate::error::LexError::InvalidNumber { text, position } => {
            (*position, format!("Invalid number format '{}'", text))
        }
        crate::error::LexError::InvalidEscape { sequence, position } => {
            (*position, format!("Invalid escape sequence '{}'", sequence))
        }
    };

    let range = position..position + 1;

    Report::build(ReportKind::Error, source_name.to_string(), position)
        .with_message(format!("Lexer Error: {}", error))
        .with_label(
            Label::new((source_name.to_string(), range))
                .with_message(message.fg(Color::Red))
                .with_color(Color::Red),
        )
        .finish()
}

/// Print a pretty error report to stderr
pub fn print_error(error: &FarrowError, source_name: &str, source_code: &str) {
    let report = create_error_report(error, source_name, source_code);
    let source = Source::from(source_code);

    report
        .eprint((source_name.to_string(), source))
        .unwrap_or_else(|e| {
            eprintln!("Failed to print error report: {}", e);
            eprintln!("Original error: {}", error);
        });
}

/// Create a colorized success message
pub fn print_success(message: &str) {
    println!("{} {}", "✅".fg(Color::Green), message);
}

/// Create a colorized info message
pub fn print_info(message: &str) {
    println!("{} {}", "ℹ️".fg(Color::Blue), message);
}

/// Create a colorized warning message
pub fn print_warning(message: &str) {
    println!("{} {}", "⚠️".fg(Color::Yellow), message);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::RuntimeError;

    #[test]
    fn test_create_runtime_error_report() {
        let error = FarrowError::Runtime(RuntimeError::unbound_variable_at(
            "x".to_string(),
            crate::ast::Span::new(10, 11),
        ));

        let source = "let y := x + 1";
        let report = create_error_report(&error, "test.fro", source);

        // Just ensure it doesn't panic
        assert!(format!("{:?}", report).contains("Runtime Error"));
    }

    #[test]
    fn test_print_utilities() {
        // These should not panic
        print_success("Test passed");
        print_info("Information message");
        print_warning("Warning message");
    }
}
