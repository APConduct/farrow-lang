use logos::Logos;

#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    // Keywords
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("case")]
    Case,
    #[token("of")]
    Of,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("import")]
    Import,
    #[token("type")]
    Type,

    // Lambda and mu symbols
    #[token("λ")]
    #[token("\\")]
    Lambda,
    #[token("μ")]
    Mu,

    // Operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("**")]
    Power,

    // Comparison
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,

    // Logical
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    Not,

    // Assignment and arrows
    #[token(":=")]
    Assign,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("|->")]
    LambdaArrow,

    // List operations
    #[token("<>")]
    Append,
    #[token(":")]
    Cons,

    // Pipe operator
    #[token("|>")]
    Pipe,

    // Delimiters
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,

    // Punctuation
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token("|")]
    Pipe_,
    #[token("_", priority = 2)]
    Wildcard,

    // Literals
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    Integer(i64),

    // Removed Float to avoid Hash/Eq issues for now
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        // Remove surrounding quotes and handle escape sequences
        let unquoted = &s[1..s.len()-1];
        Some(unquoted.replace("\\\"", "\"")
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("\\\\", "\\"))
    })]
    String(String),

    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_?]*", |lex| lex.slice().to_string(), priority = 1)]
    Identifier(String),

    // Comments (to be skipped)
    #[regex(r"--[^\n]*", logos::skip)]
    LineComment,

    #[regex(r"\{-([^-]|-[^}])*-\}", logos::skip)]
    BlockComment,

    // Error handling
    Error,
}

impl Token {
    /// Get the precedence of a binary operator token
    pub fn precedence(&self) -> Option<u8> {
        match self {
            Token::Pipe => Some(1),
            Token::Or => Some(2),
            Token::And => Some(3),
            Token::Equal
            | Token::NotEqual
            | Token::Less
            | Token::LessEqual
            | Token::Greater
            | Token::GreaterEqual => Some(4),
            Token::Cons => Some(5),
            Token::Append => Some(6),
            Token::Plus | Token::Minus => Some(7),
            Token::Star | Token::Slash | Token::Percent => Some(8),
            Token::Power => Some(9),
            _ => None,
        }
    }

    /// Check if this is a right-associative operator
    pub fn is_right_associative(&self) -> bool {
        matches!(self, Token::Power | Token::Cons | Token::Arrow)
    }

    /// Check if this token can start an expression
    pub fn can_start_expr(&self) -> bool {
        matches!(
            self,
            Token::Identifier(_)
                | Token::Integer(_)
                | Token::String(_)
                | Token::True
                | Token::False
                | Token::Lambda
                | Token::Mu
                | Token::Let
                | Token::Case
                | Token::If
                | Token::LeftParen
                | Token::LeftBracket
                | Token::LeftBrace
                | Token::Minus
                | Token::Not
                | Token::Wildcard
        )
    }

    /// Check if this is a literal token
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            Token::Integer(_) | Token::String(_) | Token::True | Token::False
        )
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Let => write!(f, "let"),
            Token::In => write!(f, "in"),
            Token::Case => write!(f, "case"),
            Token::Of => write!(f, "of"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Import => write!(f, "import"),
            Token::Type => write!(f, "type"),
            Token::Lambda => write!(f, "λ"),
            Token::Mu => write!(f, "μ"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Power => write!(f, "**"),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::Not => write!(f, "!"),
            Token::Assign => write!(f, ":="),
            Token::Arrow => write!(f, "->"),
            Token::FatArrow => write!(f, "=>"),
            Token::LambdaArrow => write!(f, "|->"),
            Token::Append => write!(f, "<>"),
            Token::Cons => write!(f, ":"),
            Token::Pipe => write!(f, "|>"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Pipe_ => write!(f, "|"),
            Token::Wildcard => write!(f, "_"),
            Token::Integer(n) => write!(f, "{}", n),
            // Token::Float(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Identifier(s) => write!(f, "{}", s),
            Token::LineComment => write!(f, "-- comment"),
            Token::BlockComment => write!(f, "{{- comment -}}"),
            Token::Error => write!(f, "ERROR"),
        }
    }
}

/// Tokenize input string and return vector of tokens with their spans
pub fn tokenize(
    input: &str,
) -> Result<Vec<(Token, std::ops::Range<usize>)>, Vec<(String, std::ops::Range<usize>)>> {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    let mut lexer = Token::lexer(input);

    while let Some(token) = lexer.next() {
        match token {
            Ok(tok) => tokens.push((tok, lexer.span())),
            Err(_) => errors.push((
                format!("Unexpected character: '{}'", lexer.slice()),
                lexer.span(),
            )),
        }
    }

    if errors.is_empty() {
        Ok(tokens)
    } else {
        Err(errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_simple() {
        let input = "x := 42";
        let tokens = tokenize(input).unwrap();
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].0, Token::Identifier("x".to_string()));
        assert_eq!(tokens[1].0, Token::Assign);
        assert_eq!(tokens[2].0, Token::Integer(42));
    }

    #[test]
    fn test_tokenize_lambda() {
        let input = "λx -> x + 1";
        let tokens = tokenize(input).unwrap();
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].0, Token::Lambda);
        assert_eq!(tokens[1].0, Token::Identifier("x".to_string()));
        assert_eq!(tokens[2].0, Token::Arrow);
    }

    #[test]
    fn test_tokenize_string() {
        let input = r#""hello world""#;
        let tokens = tokenize(input).unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].0, Token::String("hello world".to_string()));
    }

    #[test]
    fn test_tokenize_comments() {
        let input = "x -- this is a comment\ny";
        let tokens = tokenize(input).unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].0, Token::Identifier("x".to_string()));
        assert_eq!(tokens[1].0, Token::Identifier("y".to_string()));
    }

    #[test]
    fn test_tokenize_operators() {
        let input = "+ - * / % ** == != < <= > >= && || ! <>";
        let tokens = tokenize(input).unwrap();
        let expected = vec![
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::Slash,
            Token::Percent,
            Token::Power,
            Token::Equal,
            Token::NotEqual,
            Token::Less,
            Token::LessEqual,
            Token::Greater,
            Token::GreaterEqual,
            Token::And,
            Token::Or,
            Token::Not,
            Token::Append,
        ];

        for (i, expected_token) in expected.iter().enumerate() {
            assert_eq!(tokens[i].0, *expected_token);
        }
    }
}
