use chumsky::prelude::*;

use crate::ast::{BinOp, Expr, Literal, Span, Spanned, SpannedExpr};
use crate::lexer::Token;

type ParserError = Simple<Token>;

// Helper function to create spanned expressions with dummy spans
fn spanned<T>(node: T, _span: std::ops::Range<usize>) -> Spanned<T> {
    Spanned::new(node, Span::new(0, 0))
}

// Simple helper to create a spanned expression with dummy span
fn dummy_spanned<T>(node: T) -> Spanned<T> {
    Spanned::new(node, Span::new(0, 0))
}

// Parse literals
fn literal() -> impl Parser<Token, Literal, Error = ParserError> + Clone {
    select! {
        Token::Integer(n) => Literal::Int(n),
        Token::String(s) => Literal::String(s),
        Token::True => Literal::Bool(true),
        Token::False => Literal::Bool(false),
    }
}

// Parse identifiers
fn identifier() -> impl Parser<Token, String, Error = ParserError> + Clone {
    select! {
        Token::Identifier(name) => name,
    }
}

// Parse expressions
fn expr() -> impl Parser<Token, SpannedExpr, Error = ParserError> + Clone {
    recursive(|expr| {
        // Atomic expressions
        let lit = literal().map(Expr::Lit).map(dummy_spanned);

        let var = identifier().map(Expr::Var).map(dummy_spanned);

        let list = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
            .map(Expr::List)
            .map(dummy_spanned);

        let parens = expr
            .clone()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));

        // Let expression: let x := value in body
        let let_expr = just(Token::Let)
            .ignore_then(identifier())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((name, value), body)| Expr::Let {
                name,
                value: Box::new(value),
                body: Box::new(body),
            })
            .map(dummy_spanned);

        // Lambda expression: λx -> body
        let lambda = just(Token::Lambda)
            .ignore_then(identifier())
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map(|(param, body)| Expr::Lambda {
                param,
                body: Box::new(body),
            })
            .map(dummy_spanned);

        // Atomic expressions (highest precedence)
        let atom = choice((
            lit, list, let_expr, lambda, parens,
            var, // Variable should come last to avoid conflicts
        ));

        // Function application (left-associative)
        let application = atom
            .clone()
            .then(atom.clone().repeated())
            .foldl(|func, arg| {
                let span = Span::new(func.span.start, arg.span.end);
                Spanned::new(
                    Expr::Apply {
                        func: Box::new(func),
                        arg: Box::new(arg),
                    },
                    span,
                )
            });

        // Binary operations - simplified precedence
        let additive = application
            .clone()
            .then(
                choice((
                    just(Token::Plus).to(BinOp::Add),
                    just(Token::Minus).to(BinOp::Sub),
                ))
                .then(application.clone())
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                let span = Span::new(lhs.span.start, rhs.span.end);
                Spanned::new(
                    Expr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    span,
                )
            });

        let multiplicative = additive
            .clone()
            .then(
                choice((
                    just(Token::Star).to(BinOp::Mul),
                    just(Token::Slash).to(BinOp::Div),
                ))
                .then(additive.clone())
                .repeated(),
            )
            .foldl(|lhs, (op, rhs)| {
                let span = Span::new(lhs.span.start, rhs.span.end);
                Spanned::new(
                    Expr::BinOp {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    span,
                )
            });

        multiplicative
    })
}

// Parse a single expression or declaration
fn statement() -> impl Parser<Token, SpannedExpr, Error = ParserError> + Clone {
    // Variable assignment: x := expr (treated as let x := expr in x)
    let assignment = identifier()
        .then_ignore(just(Token::Assign))
        .then(expr())
        .map(|(name, value)| {
            let var_expr = Spanned::new(
                Expr::Var(name.clone()),
                Span::new(value.span.end, value.span.end),
            );
            Expr::Let {
                name,
                value: Box::new(value),
                body: Box::new(var_expr),
            }
        })
        .map_with_span(spanned);

    choice((assignment, expr()))
}

// Parse a program (multiple statements)
fn program() -> impl Parser<Token, Vec<SpannedExpr>, Error = ParserError> + Clone {
    statement()
        .separated_by(just(Token::Semicolon).or_not())
        .allow_trailing()
        .then_ignore(end())
}

// Public parsing functions
pub fn parse_expression(
    tokens: &[(Token, std::ops::Range<usize>)],
) -> Result<SpannedExpr, Vec<ParserError>> {
    let input: Vec<_> = tokens.iter().map(|(token, _)| token.clone()).collect();

    expr().then_ignore(end()).parse(input.as_slice())
}

pub fn parse_program(
    tokens: &[(Token, std::ops::Range<usize>)],
) -> Result<Vec<SpannedExpr>, Vec<ParserError>> {
    let input: Vec<_> = tokens.iter().map(|(token, _)| token.clone()).collect();

    program().parse(input.as_slice())
}

// Convenience functions that handle tokenization
pub fn parse_expr_from_str(input: &str) -> Result<SpannedExpr, String> {
    let tokens =
        crate::lexer::tokenize(input).map_err(|errors| format!("Lexer errors: {:?}", errors))?;

    parse_expression(&tokens).map_err(|errors| format!("Parser errors: {:?}", errors))
}

pub fn parse_program_from_str(input: &str) -> Result<Vec<SpannedExpr>, String> {
    let tokens =
        crate::lexer::tokenize(input).map_err(|errors| format!("Lexer errors: {:?}", errors))?;

    parse_program(&tokens).map_err(|errors| format!("Parser errors: {:?}", errors))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_literal() {
        let result = parse_expr_from_str("42");
        assert!(result.is_ok());
        match result.unwrap().node {
            Expr::Lit(Literal::Int(42)) => {}
            _ => panic!("Expected integer literal"),
        }
    }

    #[test]
    fn test_parse_variable() {
        let result = parse_expr_from_str("x");
        assert!(result.is_ok());
        match result.unwrap().node {
            Expr::Var(name) if name == "x" => {}
            _ => panic!("Expected variable"),
        }
    }

    #[test]
    fn test_parse_binary_op() {
        let result = parse_expr_from_str("1 + 2");
        assert!(result.is_ok());
        match result.unwrap().node {
            Expr::BinOp { op: BinOp::Add, .. } => {}
            _ => panic!("Expected binary operation"),
        }
    }

    #[test]
    fn test_parse_assignment() {
        let result = parse_expr_from_str("x := 42");
        assert!(result.is_ok());
        match result.unwrap().node {
            Expr::Let { name, .. } if name == "x" => {}
            _ => panic!("Expected let expression"),
        }
    }
}
