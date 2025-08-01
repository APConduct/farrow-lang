use crate::ast::{
    BinOp, Constructor, Decl, Expr, Literal, Module, Pattern, Span, Spanned, SpannedExpr,
    SpannedPattern, Type, TypeDef,
};
use crate::lexer::{OrderedFloat, Token};

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<(Token, std::ops::Range<usize>)>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<(Token, std::ops::Range<usize>)>) -> Self {
        Self { tokens, current: 0 }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> &Token {
        self.tokens
            .get(self.current)
            .map(|(token, _)| token)
            .unwrap_or(&Token::Error)
    }

    fn previous(&self) -> &(Token, std::ops::Range<usize>) {
        &self.tokens[self.current - 1]
    }

    fn current_span(&self) -> Span {
        if let Some((_, range)) = self.tokens.get(self.current) {
            Span::new(range.start, range.end)
        } else {
            Span::new(0, 0)
        }
    }

    fn previous_span(&self) -> Span {
        if self.current > 0 {
            let (_, range) = &self.tokens[self.current - 1];
            Span::new(range.start, range.end)
        } else {
            Span::new(0, 0)
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        &self.previous().0
    }

    fn check(&self, token_type: &Token) -> bool {
        if self.is_at_end() {
            false
        } else {
            std::mem::discriminant(self.peek()) == std::mem::discriminant(token_type)
        }
    }

    fn match_token(&mut self, token_type: &Token) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn spanned<T>(&self, node: T) -> Spanned<T> {
        Spanned::new(node, self.previous_span())
    }

    fn spanned_at<T>(&self, node: T, start: usize, end: usize) -> Spanned<T> {
        Spanned::new(node, Span::new(start, end))
    }

    pub fn parse_expression(&mut self) -> Result<SpannedExpr, String> {
        self.logical_or()
    }

    pub fn parse_module(&mut self) -> Result<Module, String> {
        let mut declarations = Vec::new();

        // Parse declarations and expressions
        while !self.is_at_end() {
            if self.check(&Token::Type) || self.check(&Token::Data) {
                declarations.push(self.parse_type_declaration()?);
            } else if self.can_start_declaration() {
                declarations.push(self.parse_value_declaration()?);
            } else {
                // If there's a remaining expression, treat it as main
                break;
            }
        }

        // Check if there's a main expression left
        let main_expr = if !self.is_at_end() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        Ok(Module {
            declarations,
            main_expr,
        })
    }

    fn can_start_declaration(&self) -> bool {
        if let Token::Identifier(name) = self.peek() {
            // Look ahead for := to distinguish from regular expressions
            // Also check that it's a lowercase identifier (not a constructor)
            if name.chars().next().unwrap().is_lowercase() {
                if let Some((Token::Assign, _)) = self.tokens.get(self.current + 1) {
                    return true;
                }
            }
        }
        false
    }

    fn parse_type_declaration(&mut self) -> Result<Decl, String> {
        // Parse: type Maybe a = Nothing | Just a
        if !self.match_token(&Token::Type) && !self.match_token(&Token::Data) {
            return Err("Expected 'type' or 'data' keyword".to_string());
        }

        // Parse type name
        let type_name = if let Token::Identifier(name) = self.peek() {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err("Expected type name after 'type'".to_string());
        };

        // Parse type parameters (optional)
        let mut type_params = Vec::new();
        while let Token::Identifier(param) = self.peek() {
            if param.chars().next().unwrap().is_lowercase() {
                // Check if this is actually a type parameter or constructor name
                // by looking ahead to see if we have '=' or '|' next
                let mut lookahead = self.current + 1;
                let mut is_type_param = true;

                while lookahead < self.tokens.len() {
                    match &self.tokens[lookahead].0 {
                        Token::Equals | Token::Pipe_ => break,
                        Token::Identifier(_) => {
                            // Another identifier might mean this is a constructor
                            lookahead += 1;
                        }
                        _ => {
                            is_type_param = false;
                            break;
                        }
                    }
                }

                if is_type_param {
                    type_params.push(param.clone());
                    self.advance();
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        // Expect '='
        if !self.match_token(&Token::Equals) {
            return Err("Expected '=' after type name and parameters".to_string());
        }

        // Parse constructors
        let mut constructors = Vec::new();

        // First constructor
        constructors.push(self.parse_constructor()?);

        // Additional constructors separated by '|'
        while self.match_token(&Token::Pipe_) {
            constructors.push(self.parse_constructor()?);
        }

        Ok(Decl::Type {
            name: type_name,
            params: type_params,
            definition: TypeDef::Sum(constructors),
        })
    }

    fn parse_constructor(&mut self) -> Result<Constructor, String> {
        let name = if let Token::Identifier(name) = self.peek() {
            if name.chars().next().unwrap().is_uppercase() {
                let name = name.clone();
                self.advance();
                name
            } else {
                return Err("Constructor names must start with uppercase".to_string());
            }
        } else {
            return Err("Expected constructor name".to_string());
        };

        // Parse field types
        let mut fields = Vec::new();
        while !self.is_at_end()
            && !self.check(&Token::Pipe_)
            && !self.check(&Token::Type)
            && !self.check(&Token::Data)
            && !self.check(&Token::Identifier("".into())) // Don't consume identifiers that might start next declaration
            && self.can_start_type()
        {
            // Check if we're at a lowercase identifier followed by :=, which would be a value declaration
            if let Token::Identifier(id) = self.peek() {
                if id.chars().next().unwrap().is_lowercase() {
                    // Look ahead for :=
                    if let Some((Token::Assign, _)) = self.tokens.get(self.current + 1) {
                        break; // This is a value declaration, stop parsing fields
                    }
                }

                // If it's an uppercase identifier, check if it's the next constructor
                if id.chars().next().unwrap().is_uppercase() {
                    // Look ahead to see if this is followed by '|' or another constructor pattern
                    let mut lookahead = self.current + 1;
                    let mut is_next_constructor = false;

                    // If we see nothing after this identifier, or we see a pipe, it's likely next constructor
                    if lookahead >= self.tokens.len() {
                        is_next_constructor = true;
                    } else {
                        match &self.tokens[lookahead].0 {
                            Token::Pipe_ => is_next_constructor = true,
                            Token::Identifier(next_id)
                                if next_id.chars().next().unwrap().is_lowercase()
                                    && self.tokens.get(lookahead + 1).map(|(t, _)| t)
                                        == Some(&Token::Assign) =>
                            {
                                is_next_constructor = true; // Next is a value declaration
                            }
                            _ => {}
                        }
                    }

                    if is_next_constructor {
                        break;
                    }
                }
            }

            // For now, just parse simple type names without full type parsing
            if let Token::Identifier(type_name) = self.peek() {
                let type_name = type_name.clone();
                self.advance();
                fields.push(Type::Named(type_name));
            } else {
                fields.push(self.parse_type()?);
            }
        }

        Ok(Constructor { name, fields })
    }

    fn can_start_type(&self) -> bool {
        matches!(
            self.peek(),
            Token::Identifier(_) | Token::LeftParen | Token::LeftBracket
        )
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        if let Token::Identifier(name) = self.peek() {
            let name = name.clone();
            self.advance();

            if name.chars().next().unwrap().is_lowercase() {
                // Type variable
                Ok(Type::Var(name))
            } else {
                // Named type - don't parse arguments for now to avoid confusion
                Ok(Type::Named(name))
            }
        } else if self.match_token(&Token::LeftParen) {
            let ty = self.parse_type()?;
            if self.match_token(&Token::RightParen) {
                Ok(ty)
            } else {
                Err("Expected ')' after type".to_string())
            }
        } else if self.match_token(&Token::LeftBracket) {
            let element_type = self.parse_type()?;
            if self.match_token(&Token::RightBracket) {
                Ok(Type::List(Box::new(element_type)))
            } else {
                Err("Expected ']' after list type".to_string())
            }
        } else {
            Err("Expected type".to_string())
        }
    }

    fn parse_value_declaration(&mut self) -> Result<Decl, String> {
        // Parse: name := expression
        let name = if let Token::Identifier(name) = self.peek() {
            let name = name.clone();
            self.advance();
            name
        } else {
            return Err("Expected identifier in value declaration".to_string());
        };

        if !self.match_token(&Token::Assign) {
            return Err("Expected ':=' after identifier in value declaration".to_string());
        }

        let value = self.parse_expression()?;

        Ok(Decl::Value {
            name,
            type_annotation: None,
            value,
        })
    }

    fn logical_or(&mut self) -> Result<SpannedExpr, String> {
        let mut expr = self.logical_and()?;

        while self.match_token(&Token::Or) {
            let rhs = self.logical_and()?;
            expr = self.spanned(Expr::BinOp {
                op: BinOp::Or,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<SpannedExpr, String> {
        let mut expr = self.equality()?;

        while self.match_token(&Token::And) {
            let rhs = self.equality()?;
            expr = self.spanned(Expr::BinOp {
                op: BinOp::And,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<SpannedExpr, String> {
        let mut expr = self.comparison()?;

        while matches!(self.peek(), Token::Equal | Token::NotEqual) {
            let op = match self.advance() {
                Token::Equal => BinOp::Eq,
                Token::NotEqual => BinOp::Neq,
                _ => unreachable!(),
            };
            let rhs = self.comparison()?;
            expr = self.spanned(Expr::BinOp {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<SpannedExpr, String> {
        let mut expr = self.term()?;

        while matches!(
            self.peek(),
            Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual
        ) {
            let op = match self.advance() {
                Token::Greater => BinOp::Gt,
                Token::GreaterEqual => BinOp::Ge,
                Token::Less => BinOp::Lt,
                Token::LessEqual => BinOp::Le,
                _ => unreachable!(),
            };
            let rhs = self.term()?;
            expr = self.spanned(Expr::BinOp {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<SpannedExpr, String> {
        let mut expr = self.factor()?;

        while matches!(self.peek(), Token::Minus | Token::Plus) {
            let op = match self.advance() {
                Token::Minus => BinOp::Sub,
                Token::Plus => BinOp::Add,
                _ => unreachable!(),
            };
            let rhs = self.factor()?;
            expr = self.spanned(Expr::BinOp {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<SpannedExpr, String> {
        let mut expr = self.cons()?;

        while matches!(self.peek(), Token::Slash | Token::Star | Token::Percent) {
            let op = match self.advance() {
                Token::Slash => BinOp::Div,
                Token::Star => BinOp::Mul,
                Token::Percent => BinOp::Mod,
                _ => unreachable!(),
            };
            let rhs = self.cons()?;
            expr = self.spanned(Expr::BinOp {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }

        Ok(expr)
    }

    fn cons(&mut self) -> Result<SpannedExpr, String> {
        let mut expr = self.pipe()?;

        if self.match_token(&Token::Cons) {
            let tail = self.cons()?; // Right associative
            expr = self.spanned(Expr::Cons {
                head: Box::new(expr),
                tail: Box::new(tail),
            });
        }

        Ok(expr)
    }

    fn pipe(&mut self) -> Result<SpannedExpr, String> {
        let mut expr = self.unary()?;

        while self.match_token(&Token::Pipe) {
            let rhs = self.unary()?;
            expr = self.spanned(Expr::BinOp {
                op: BinOp::Pipe,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            });
        }

        Ok(expr)
    }

    fn application(&mut self) -> Result<SpannedExpr, String> {
        let mut expr = self.primary()?;

        while !self.is_at_end()
            && !matches!(
                self.peek(),
                Token::RightParen
                    | Token::RightBracket
                    | Token::RightBrace
                    | Token::Semicolon
                    | Token::Comma
                    | Token::Plus
                    | Token::Minus
                    | Token::Star
                    | Token::Slash
                    | Token::Percent
                    | Token::Equal
                    | Token::NotEqual
                    | Token::Less
                    | Token::LessEqual
                    | Token::Greater
                    | Token::GreaterEqual
                    | Token::And
                    | Token::Or
                    | Token::Cons
                    | Token::Pipe
                    | Token::FatArrow
                    | Token::In
                    | Token::Then
                    | Token::Else
                    | Token::Of
                    | Token::Wildcard // Don't consume wildcard patterns as function arguments
            )
            && self.can_start_primary()
        {
            let arg = self.primary()?;
            expr = self.spanned(Expr::Apply {
                func: Box::new(expr),
                arg: Box::new(arg),
            });
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<SpannedExpr, String> {
        if let Token::Integer(n) = self.peek() {
            let n = *n;
            self.advance();
            return Ok(self.spanned(Expr::Lit(Literal::Int(n))));
        }

        if let Token::Float(OrderedFloat(f)) = self.peek() {
            let f = *f;
            self.advance();
            return Ok(self.spanned(Expr::Lit(Literal::Float(f))));
        }

        if let Token::String(s) = self.peek() {
            let s = s.clone();
            self.advance();
            return Ok(self.spanned(Expr::Lit(Literal::String(s))));
        }

        if self.match_token(&Token::True) {
            return Ok(self.spanned(Expr::Lit(Literal::Bool(true))));
        }

        if self.match_token(&Token::False) {
            return Ok(self.spanned(Expr::Lit(Literal::Bool(false))));
        }

        if let Token::Identifier(name) = self.peek() {
            let name = name.clone();
            self.advance();

            // Check for lambda: x |-> body
            if self.match_token(&Token::LambdaArrow) {
                let body = self.parse_expression()?;
                return Ok(self.spanned(Expr::Lambda {
                    param: name,
                    body: Box::new(body),
                }));
            }

            return Ok(self.spanned(Expr::Var(name)));
        }

        if self.match_token(&Token::Lambda) {
            if let Token::Identifier(param) = self.peek() {
                let param = param.clone();
                self.advance();
                if self.match_token(&Token::Arrow) {
                    let body = self.parse_expression()?;
                    return Ok(self.spanned(Expr::Lambda {
                        param,
                        body: Box::new(body),
                    }));
                }
            }
            return Err("Expected parameter name after λ".to_string());
        }

        if self.match_token(&Token::Mu) {
            if let Token::Identifier(name) = self.peek() {
                let name = name.clone();
                self.advance();
                if self.match_token(&Token::LambdaArrow) {
                    let body = self.parse_expression()?;
                    return Ok(self.spanned(Expr::Mu {
                        name,
                        body: Box::new(body),
                    }));
                }
            }
            return Err("Expected function name after μ".to_string());
        }

        if self.match_token(&Token::Let) {
            if let Token::Identifier(name) = self.peek() {
                let name = name.clone();
                self.advance();
                if self.match_token(&Token::Assign) {
                    let value = self.parse_expression()?;
                    if self.match_token(&Token::In) {
                        let body = self.parse_expression()?;
                        return Ok(self.spanned(Expr::Let {
                            name,
                            value: Box::new(value),
                            body: Box::new(body),
                        }));
                    }
                }
            }
            return Err("Invalid let expression".to_string());
        }

        if self.match_token(&Token::Case) {
            let scrutinee = self.parse_expression()?;
            if self.match_token(&Token::Of) {
                let mut branches = Vec::new();

                loop {
                    // Check if we've reached the end of the case expression
                    if self.is_at_end()
                        || self.check(&Token::RightParen)
                        || self.check(&Token::In)
                        || self.check(&Token::Then)
                        || self.check(&Token::Else)
                    {
                        break;
                    }

                    let pattern = self.parse_pattern()?;
                    if self.match_token(&Token::FatArrow) {
                        let expr = self.parse_expression()?;
                        branches.push((pattern, expr));

                        // Optional comma - if present, consume it and continue
                        if self.match_token(&Token::Comma) {
                            continue;
                        }

                        // If no comma, check if we can parse another pattern
                        // This handles the case where branches are separated by newlines/whitespace only
                        if !self.is_at_end()
                            && !self.check(&Token::RightParen)
                            && !self.check(&Token::In)
                            && !self.check(&Token::Then)
                            && !self.check(&Token::Else)
                            && !self.check(&Token::RightBrace)
                            && !self.check(&Token::RightBracket)
                            && self.can_start_pattern()
                        {
                            continue;
                        }

                        // No more patterns, end of case expression
                        break;
                    } else {
                        return Err("Expected '=>' after pattern".to_string());
                    }
                }

                return Ok(self.spanned(Expr::Case {
                    scrutinee: Box::new(scrutinee),
                    branches,
                }));
            }
            return Err("Expected 'of' after case expression".to_string());
        }

        if self.match_token(&Token::If) {
            let condition = self.parse_expression()?;
            if self.match_token(&Token::Then) {
                let then_branch = self.parse_expression()?;
                if self.match_token(&Token::Else) {
                    let else_branch = self.parse_expression()?;
                    return Ok(self.spanned(Expr::If {
                        condition: Box::new(condition),
                        then_branch: Box::new(then_branch),
                        else_branch: Box::new(else_branch),
                    }));
                }
            }
            return Err("Invalid if expression".to_string());
        }

        if self.match_token(&Token::LeftParen) {
            // Check for unit literal ()
            if self.match_token(&Token::RightParen) {
                return Ok(self.spanned(Expr::Lit(Literal::Unit)));
            }

            let expr = self.parse_expression()?;
            if self.match_token(&Token::RightParen) {
                return Ok(expr);
            }
            return Err("Expected ')' after expression".to_string());
        }

        if self.match_token(&Token::LeftBracket) {
            let mut elements = Vec::new();

            if !self.check(&Token::RightBracket) {
                loop {
                    elements.push(self.parse_expression()?);
                    if !self.match_token(&Token::Comma) {
                        break;
                    }
                }
            }

            if self.match_token(&Token::RightBracket) {
                return Ok(self.spanned(Expr::List(elements)));
            }
            return Err("Expected ']' after list elements".to_string());
        }

        if self.match_token(&Token::LeftBrace) {
            let mut expressions = Vec::new();

            // Handle empty block
            if self.match_token(&Token::RightBrace) {
                return Ok(self.spanned(Expr::Block(expressions)));
            }

            // Parse expressions separated by semicolons
            loop {
                expressions.push(self.parse_block_statement()?);

                // Check for semicolon or end of block
                if self.match_token(&Token::Semicolon) {
                    // Continue parsing more expressions
                    if self.check(&Token::RightBrace) {
                        // Semicolon before closing brace is allowed
                        break;
                    }
                } else if self.check(&Token::RightBrace) {
                    // No semicolon before closing brace - last expression
                    break;
                } else {
                    return Err("Expected ';' or '}' after expression in block".to_string());
                }
            }

            if self.match_token(&Token::RightBrace) {
                return Ok(self.spanned(Expr::Block(expressions)));
            }
            return Err("Expected '}' after block expressions".to_string());
        }

        Err(format!("Unexpected token: {:?}", self.peek()))
    }

    fn can_start_pattern(&self) -> bool {
        matches!(
            self.peek(),
            Token::Wildcard
                | Token::Integer(_)
                | Token::String(_)
                | Token::True
                | Token::False
                | Token::LeftBracket
                | Token::LeftParen
                | Token::Identifier(_)
        )
    }

    fn can_start_primary(&self) -> bool {
        matches!(
            self.peek(),
            Token::Integer(_)
                | Token::Float(_)
                | Token::String(_)
                | Token::True
                | Token::False
                | Token::Identifier(_)
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
        )
    }

    fn unary(&mut self) -> Result<SpannedExpr, String> {
        if self.match_token(&Token::Minus) {
            let operand = self.unary()?;
            return Ok(self.spanned(Expr::UnaryOp {
                op: crate::ast::UnaryOp::Neg,
                operand: Box::new(operand),
            }));
        }

        if self.match_token(&Token::Not) {
            let operand = self.unary()?;
            return Ok(self.spanned(Expr::UnaryOp {
                op: crate::ast::UnaryOp::Not,
                operand: Box::new(operand),
            }));
        }

        self.application()
    }

    fn parse_block_statement(&mut self) -> Result<SpannedExpr, String> {
        // Special handling for let statements in blocks
        if self.check(&Token::Let) {
            // Look ahead to see if this is a block-style let (no 'in') or regular let-in
            let mut lookahead = self.current + 1;

            // Skip past identifier and :=
            if lookahead < self.tokens.len()
                && matches!(self.tokens[lookahead].0, Token::Identifier(_))
            {
                lookahead += 1;
                if lookahead < self.tokens.len()
                    && matches!(self.tokens[lookahead].0, Token::Assign)
                {
                    lookahead += 1;

                    // Skip past the value expression to find 'in' or end
                    let mut paren_depth = 0;
                    let mut bracket_depth = 0;
                    let mut brace_depth = 0;

                    while lookahead < self.tokens.len() {
                        match &self.tokens[lookahead].0 {
                            Token::LeftParen => paren_depth += 1,
                            Token::RightParen => paren_depth -= 1,
                            Token::LeftBracket => bracket_depth += 1,
                            Token::RightBracket => bracket_depth -= 1,
                            Token::LeftBrace => brace_depth += 1,
                            Token::RightBrace => {
                                brace_depth -= 1;
                                if brace_depth < 0 && paren_depth == 0 && bracket_depth == 0 {
                                    // We've reached the end of the block
                                    break;
                                }
                            }
                            Token::In
                                if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 =>
                            {
                                // Found 'in' at top level - this is a regular let-in expression
                                return self.parse_expression();
                            }
                            Token::Semicolon
                                if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 =>
                            {
                                // Found semicolon at top level - this is a block-style let
                                break;
                            }
                            _ => {}
                        }
                        lookahead += 1;
                    }

                    // If we get here, it's likely a block-style let statement
                    self.advance(); // consume 'let'
                    if let Token::Identifier(name) = self.peek() {
                        let name = name.clone();
                        self.advance();
                        if self.match_token(&Token::Assign) {
                            let value = self.parse_expression()?;
                            // In block context, let statements don't require 'in' clause
                            // Treat as let x := value in ()
                            let unit_body = self.spanned(Expr::Lit(Literal::Unit));
                            return Ok(self.spanned(Expr::Let {
                                name,
                                value: Box::new(value),
                                body: Box::new(unit_body),
                            }));
                        }
                    }
                    return Err("Invalid let statement in block".to_string());
                }
            }
        }

        // For non-let statements or regular let-in expressions, parse as normal expression
        self.parse_expression()
    }

    fn parse_pattern(&mut self) -> Result<SpannedPattern, String> {
        if self.check(&Token::Wildcard) {
            self.advance();
            return Ok(self.spanned(Pattern::Wild));
        }

        if let Token::Integer(n) = self.peek() {
            let n = *n;
            self.advance();
            return Ok(self.spanned(Pattern::Lit(Literal::Int(n))));
        }

        if let Token::Float(OrderedFloat(f)) = self.peek() {
            let f = *f;
            self.advance();
            return Ok(self.spanned(Pattern::Lit(Literal::Float(f))));
        }

        if let Token::String(s) = self.peek() {
            let s = s.clone();
            self.advance();
            return Ok(self.spanned(Pattern::Lit(Literal::String(s))));
        }

        if self.match_token(&Token::True) {
            return Ok(self.spanned(Pattern::Lit(Literal::Bool(true))));
        }

        if self.match_token(&Token::False) {
            return Ok(self.spanned(Pattern::Lit(Literal::Bool(false))));
        }

        if self.match_token(&Token::LeftBracket) {
            let mut patterns = Vec::new();

            if !self.check(&Token::RightBracket) {
                loop {
                    patterns.push(self.parse_pattern()?);
                    if !self.match_token(&Token::Comma) {
                        break;
                    }
                }
            }

            if self.match_token(&Token::RightBracket) {
                return Ok(self.spanned(Pattern::List(patterns)));
            }
            return Err("Expected ']' after list pattern".to_string());
        }

        if self.match_token(&Token::LeftParen) {
            let pattern = self.parse_pattern()?;
            if self.match_token(&Token::RightParen) {
                return Ok(pattern);
            }
            return Err("Expected ')' after pattern".to_string());
        }

        if let Token::Identifier(name) = self.peek() {
            let name = name.clone();
            self.advance();

            // Check for cons pattern: head : tail
            if self.check(&Token::Cons) {
                self.advance();
                let tail = self.parse_pattern()?;
                return Ok(self.spanned(Pattern::Cons {
                    head: Box::new(self.spanned(Pattern::Var(name))),
                    tail: Box::new(tail),
                }));
            }

            // Check if this is a constructor (starts with uppercase)
            if name.chars().next().unwrap().is_uppercase() {
                // This is a constructor
                let mut args = Vec::new();

                // Check for constructor arguments (space-separated, not parentheses)
                while !self.is_at_end()
                    && !matches!(
                        self.peek(),
                        Token::FatArrow
                            | Token::Comma
                            | Token::RightParen
                            | Token::RightBracket
                            | Token::Pipe_
                    )
                    && self.can_start_pattern()
                {
                    args.push(self.parse_pattern()?);
                }

                return Ok(self.spanned(Pattern::Constructor { name, args }));
            }

            return Ok(self.spanned(Pattern::Var(name)));
        }

        Err(format!("Unexpected token in pattern: {:?}", self.peek()))
    }
}

pub fn parse_expr_from_str(input: &str) -> Result<SpannedExpr, String> {
    let tokens =
        crate::lexer::tokenize(input).map_err(|errors| format!("Lexer errors: {:?}", errors))?;

    let mut parser = Parser::new(tokens);
    parser.parse_expression()
}

pub fn parse_module_from_str(input: &str) -> Result<Module, String> {
    let tokens =
        crate::lexer::tokenize(input).map_err(|errors| format!("Lexer errors: {:?}", errors))?;

    let mut parser = Parser::new(tokens);
    parser.parse_module()
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
    fn test_parse_lambda() {
        let result = parse_expr_from_str("x |-> x + 1");
        assert!(result.is_ok());
        match result.unwrap().node {
            Expr::Lambda { param, .. } if param == "x" => {}
            _ => panic!("Expected lambda expression"),
        }
    }

    #[test]
    fn test_parse_mu() {
        let result = parse_expr_from_str("μf |-> (n |-> n + 1)");
        assert!(result.is_ok());
        match result.unwrap().node {
            Expr::Mu { name, .. } if name == "f" => {}
            _ => panic!("Expected mu expression"),
        }
    }

    #[test]
    fn test_parse_case() {
        let result = parse_expr_from_str("case x of 0 => 1, _ => 2");
        assert!(result.is_ok());
        match result.unwrap().node {
            Expr::Case { branches, .. } => {
                assert_eq!(branches.len(), 2);
            }
            _ => panic!("Expected case expression"),
        }
    }

    #[test]
    fn test_parse_cons() {
        let result = parse_expr_from_str("1 : []");
        assert!(result.is_ok());
        match result.unwrap().node {
            Expr::Cons { .. } => {}
            _ => panic!("Expected cons expression"),
        }
    }
}
