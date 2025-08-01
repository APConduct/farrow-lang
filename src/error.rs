use crate::ast::Span;
use std::fmt;

/// Runtime errors that can occur during evaluation
#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    /// Variable not found in environment
    UnboundVariable { name: String, span: Option<Span> },
    /// Type mismatch during operation
    TypeMismatch {
        expected: String,
        found: String,
        span: Option<Span>,
    },
    /// Division by zero
    DivisionByZero { span: Option<Span> },
    /// No pattern matched in case expression
    PatternMatchFailure { span: Option<Span> },
    /// Attempted to apply a non-function value
    InvalidApplication {
        value_type: String,
        span: Option<Span>,
    },
    /// Function called with wrong number of arguments
    ArityMismatch {
        expected: usize,
        found: usize,
        span: Option<Span>,
    },
    /// Index out of bounds for list operations
    IndexOutOfBounds {
        index: i64,
        length: usize,
        span: Option<Span>,
    },
    /// Empty list operation (head/tail on empty list)
    EmptyList {
        operation: String,
        span: Option<Span>,
    },
    /// Stack overflow from too deep recursion
    StackOverflow { span: Option<Span> },
    /// Built-in function error
    BuiltinError {
        function: String,
        message: String,
        span: Option<Span>,
    },
    /// Custom error with message
    Custom { message: String, span: Option<Span> },
}

/// Parse errors that can occur during parsing
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    /// Unexpected token
    UnexpectedToken {
        expected: String,
        found: String,
        span: Span,
    },
    /// Unexpected end of input
    UnexpectedEof { expected: String, span: Span },
    /// Invalid syntax
    InvalidSyntax { message: String, span: Span },
    /// Lexer error
    LexError { message: String, span: Span },
}

/// Lexer errors
#[derive(Debug, Clone, PartialEq)]
pub enum LexError {
    /// Invalid character
    InvalidCharacter { character: char, position: usize },
    /// Unterminated string literal
    UnterminatedString { start: usize },
    /// Invalid number format
    InvalidNumber { text: String, position: usize },
    /// Invalid escape sequence
    InvalidEscape { sequence: String, position: usize },
}

/// All possible errors in the Farrow interpreter
#[derive(Debug, Clone, PartialEq)]
pub enum FarrowError {
    Runtime(RuntimeError),
    Parse(ParseError),
    Lex(LexError),
}

impl RuntimeError {
    pub fn unbound_variable(name: String) -> Self {
        RuntimeError::UnboundVariable { name, span: None }
    }

    pub fn unbound_variable_at(name: String, span: Span) -> Self {
        RuntimeError::UnboundVariable {
            name,
            span: Some(span),
        }
    }

    pub fn type_mismatch(expected: &str, found: &str) -> Self {
        RuntimeError::TypeMismatch {
            expected: expected.to_string(),
            found: found.to_string(),
            span: None,
        }
    }

    pub fn type_mismatch_at(expected: &str, found: &str, span: Span) -> Self {
        RuntimeError::TypeMismatch {
            expected: expected.to_string(),
            found: found.to_string(),
            span: Some(span),
        }
    }

    pub fn division_by_zero() -> Self {
        RuntimeError::DivisionByZero { span: None }
    }

    pub fn division_by_zero_at(span: Span) -> Self {
        RuntimeError::DivisionByZero { span: Some(span) }
    }

    pub fn pattern_match_failure() -> Self {
        RuntimeError::PatternMatchFailure { span: None }
    }

    pub fn pattern_match_failure_at(span: Span) -> Self {
        RuntimeError::PatternMatchFailure { span: Some(span) }
    }

    pub fn invalid_application(value_type: &str) -> Self {
        RuntimeError::InvalidApplication {
            value_type: value_type.to_string(),
            span: None,
        }
    }

    pub fn invalid_application_at(value_type: &str, span: Span) -> Self {
        RuntimeError::InvalidApplication {
            value_type: value_type.to_string(),
            span: Some(span),
        }
    }

    pub fn arity_mismatch(expected: usize, found: usize) -> Self {
        RuntimeError::ArityMismatch {
            expected,
            found,
            span: None,
        }
    }

    pub fn arity_mismatch_at(expected: usize, found: usize, span: Span) -> Self {
        RuntimeError::ArityMismatch {
            expected,
            found,
            span: Some(span),
        }
    }

    pub fn empty_list(operation: &str) -> Self {
        RuntimeError::EmptyList {
            operation: operation.to_string(),
            span: None,
        }
    }

    pub fn empty_list_at(operation: &str, span: Span) -> Self {
        RuntimeError::EmptyList {
            operation: operation.to_string(),
            span: Some(span),
        }
    }

    pub fn stack_overflow() -> Self {
        RuntimeError::StackOverflow { span: None }
    }

    pub fn stack_overflow_at(span: Span) -> Self {
        RuntimeError::StackOverflow { span: Some(span) }
    }

    pub fn builtin_error(function: &str, message: &str) -> Self {
        RuntimeError::BuiltinError {
            function: function.to_string(),
            message: message.to_string(),
            span: None,
        }
    }

    pub fn custom(message: &str) -> Self {
        RuntimeError::Custom {
            message: message.to_string(),
            span: None,
        }
    }

    pub fn custom_at(message: &str, span: Span) -> Self {
        RuntimeError::Custom {
            message: message.to_string(),
            span: Some(span),
        }
    }

    pub fn span(&self) -> Option<&Span> {
        match self {
            RuntimeError::UnboundVariable { span, .. } => span.as_ref(),
            RuntimeError::TypeMismatch { span, .. } => span.as_ref(),
            RuntimeError::DivisionByZero { span } => span.as_ref(),
            RuntimeError::PatternMatchFailure { span } => span.as_ref(),
            RuntimeError::InvalidApplication { span, .. } => span.as_ref(),
            RuntimeError::ArityMismatch { span, .. } => span.as_ref(),
            RuntimeError::IndexOutOfBounds { span, .. } => span.as_ref(),
            RuntimeError::EmptyList { span, .. } => span.as_ref(),
            RuntimeError::StackOverflow { span } => span.as_ref(),
            RuntimeError::BuiltinError { span, .. } => span.as_ref(),
            RuntimeError::Custom { span, .. } => span.as_ref(),
        }
    }
}

impl ParseError {
    pub fn unexpected_token(expected: &str, found: &str, span: Span) -> Self {
        ParseError::UnexpectedToken {
            expected: expected.to_string(),
            found: found.to_string(),
            span,
        }
    }

    pub fn unexpected_eof(expected: &str, span: Span) -> Self {
        ParseError::UnexpectedEof {
            expected: expected.to_string(),
            span,
        }
    }

    pub fn invalid_syntax(message: &str, span: Span) -> Self {
        ParseError::InvalidSyntax {
            message: message.to_string(),
            span,
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            ParseError::UnexpectedToken { span, .. } => span,
            ParseError::UnexpectedEof { span, .. } => span,
            ParseError::InvalidSyntax { span, .. } => span,
            ParseError::LexError { span, .. } => span,
        }
    }
}

impl LexError {
    pub fn invalid_character(character: char, position: usize) -> Self {
        LexError::InvalidCharacter {
            character,
            position,
        }
    }

    pub fn unterminated_string(start: usize) -> Self {
        LexError::UnterminatedString { start }
    }

    pub fn invalid_number(text: String, position: usize) -> Self {
        LexError::InvalidNumber { text, position }
    }

    pub fn invalid_escape(sequence: String, position: usize) -> Self {
        LexError::InvalidEscape { sequence, position }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::UnboundVariable { name, .. } => {
                write!(f, "Unbound variable: '{}'", name)
            }
            RuntimeError::TypeMismatch {
                expected, found, ..
            } => {
                write!(f, "Type error: expected {}, found {}", expected, found)
            }
            RuntimeError::DivisionByZero { .. } => {
                write!(f, "Division by zero")
            }
            RuntimeError::PatternMatchFailure { .. } => {
                write!(f, "No pattern matched in case expression")
            }
            RuntimeError::InvalidApplication { value_type, .. } => {
                write!(f, "Cannot apply {} as a function", value_type)
            }
            RuntimeError::ArityMismatch {
                expected, found, ..
            } => {
                write!(
                    f,
                    "Function expects {} arguments, but {} were provided",
                    expected, found
                )
            }
            RuntimeError::IndexOutOfBounds { index, length, .. } => {
                write!(
                    f,
                    "Index {} out of bounds for list of length {}",
                    index, length
                )
            }
            RuntimeError::EmptyList { operation, .. } => {
                write!(f, "Cannot perform {} on empty list", operation)
            }
            RuntimeError::StackOverflow { .. } => {
                write!(f, "Stack overflow: recursion too deep")
            }
            RuntimeError::BuiltinError {
                function, message, ..
            } => {
                write!(f, "Error in built-in function '{}': {}", function, message)
            }
            RuntimeError::Custom { message, .. } => {
                write!(f, "{}", message)
            }
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected, found, ..
            } => {
                write!(f, "Parse error: expected {}, found {}", expected, found)
            }
            ParseError::UnexpectedEof { expected, .. } => {
                write!(
                    f,
                    "Parse error: unexpected end of input, expected {}",
                    expected
                )
            }
            ParseError::InvalidSyntax { message, .. } => {
                write!(f, "Syntax error: {}", message)
            }
            ParseError::LexError { message, .. } => {
                write!(f, "Lexer error: {}", message)
            }
        }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::InvalidCharacter {
                character,
                position,
            } => {
                write!(
                    f,
                    "Invalid character '{}' at position {}",
                    character, position
                )
            }
            LexError::UnterminatedString { start } => {
                write!(
                    f,
                    "Unterminated string literal starting at position {}",
                    start
                )
            }
            LexError::InvalidNumber { text, position } => {
                write!(f, "Invalid number '{}' at position {}", text, position)
            }
            LexError::InvalidEscape { sequence, position } => {
                write!(
                    f,
                    "Invalid escape sequence '{}' at position {}",
                    sequence, position
                )
            }
        }
    }
}

impl fmt::Display for FarrowError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FarrowError::Runtime(err) => write!(f, "{}", err),
            FarrowError::Parse(err) => write!(f, "{}", err),
            FarrowError::Lex(err) => write!(f, "{}", err),
        }
    }
}

impl std::error::Error for RuntimeError {}
impl std::error::Error for ParseError {}
impl std::error::Error for LexError {}
impl std::error::Error for FarrowError {}

impl From<RuntimeError> for FarrowError {
    fn from(err: RuntimeError) -> Self {
        FarrowError::Runtime(err)
    }
}

impl From<ParseError> for FarrowError {
    fn from(err: ParseError) -> Self {
        FarrowError::Parse(err)
    }
}

impl From<LexError> for FarrowError {
    fn from(err: LexError) -> Self {
        FarrowError::Lex(err)
    }
}

/// Result type for Farrow operations
pub type FarrowResult<T> = Result<T, FarrowError>;
pub type RuntimeResult<T> = Result<T, RuntimeError>;
pub type ParseResult<T> = Result<T, ParseError>;
pub type LexResult<T> = Result<T, LexError>;
