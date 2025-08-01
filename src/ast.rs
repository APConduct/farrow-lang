use std::fmt;

/// Source location information for error reporting
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

/// AST node with source location
#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

pub type SpannedExpr = Spanned<Expr>;
pub type SpannedPattern = Spanned<Pattern>;

/// Main expression type
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Literals
    Lit(Literal),

    // Variables
    Var(String),

    // Function abstraction: λx -> body
    Lambda {
        param: String,
        body: Box<SpannedExpr>,
    },

    // Function application: f x
    Apply {
        func: Box<SpannedExpr>,
        arg: Box<SpannedExpr>,
    },

    // Binary operations: a + b
    BinOp {
        op: BinOp,
        lhs: Box<SpannedExpr>,
        rhs: Box<SpannedExpr>,
    },

    // Unary operations: -x
    UnaryOp {
        op: UnaryOp,
        operand: Box<SpannedExpr>,
    },

    // Let binding: let x := value in body
    Let {
        name: String,
        value: Box<SpannedExpr>,
        body: Box<SpannedExpr>,
    },

    // Recursive binding: μf -> body
    Mu {
        name: String,
        body: Box<SpannedExpr>,
    },

    // Pattern matching: case expr of patterns
    Case {
        scrutinee: Box<SpannedExpr>,
        branches: Vec<(SpannedPattern, SpannedExpr)>,
    },

    // Conditional: if cond then expr else expr
    If {
        condition: Box<SpannedExpr>,
        then_branch: Box<SpannedExpr>,
        else_branch: Box<SpannedExpr>,
    },

    // Lists
    List(Vec<SpannedExpr>),

    // List construction: head : tail
    Cons {
        head: Box<SpannedExpr>,
        tail: Box<SpannedExpr>,
    },

    // Block expression: { stmt1; stmt2; expr }
    Block(Vec<SpannedExpr>),
}

/// Literal values
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Unit,
}

/// Binary operators
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    // Comparison
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,

    // Logical
    And,
    Or,

    // List operations
    Append, // ++

    // Pipe operator
    Pipe, // |>
}

/// Unary operators
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Neg, // -x
    Not, // !x
}

/// Pattern matching patterns
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    // Wildcard pattern: _
    Wild,

    // Variable pattern: x
    Var(String),

    // Literal pattern: 42, "hello", true
    Lit(Literal),

    // List patterns
    List(Vec<SpannedPattern>),

    // Cons pattern: head : tail
    Cons {
        head: Box<SpannedPattern>,
        tail: Box<SpannedPattern>,
    },

    // Constructor pattern (for future ADTs): Some(x)
    Constructor {
        name: String,
        args: Vec<SpannedPattern>,
    },

    // Or pattern: pat1 | pat2
    Or(Vec<SpannedPattern>),

    // Guard pattern: pat if condition
    Guard {
        pattern: Box<SpannedPattern>,
        condition: Box<SpannedExpr>,
    },
}

/// Type annotations (for future type system)
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Primitive types
    Int,
    Float,
    String,
    Bool,
    Unit,

    // Function type: a -> b
    Function {
        param: Box<Type>,
        result: Box<Type>,
    },

    // List type: [a]
    List(Box<Type>),

    // Type variable: a
    Var(String),

    // Named type: Maybe, List, etc.
    Named(String),

    // Generic type application: Maybe a
    App {
        constructor: Box<Type>,
        args: Vec<Type>,
    },
}

/// Top-level declarations (for future module system)
#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    // Value declaration: x := expr
    Value {
        name: String,
        type_annotation: Option<Type>,
        value: SpannedExpr,
    },

    // Type declaration: type Name = ...
    Type {
        name: String,
        params: Vec<String>,
        definition: TypeDef,
    },

    // Import declaration: import Module
    Import {
        module: String,
        items: Option<Vec<String>>,
    },
}

/// Type definitions (for future ADTs)
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDef {
    // Type alias: type String = [Char]
    Alias(Type),

    // Sum type: type Maybe a = Nothing | Just a
    Sum(Vec<Constructor>),

    // Product type: type Point = Point Float Float
    Product(Vec<Type>),
}

/// Constructor for sum types
#[derive(Debug, Clone, PartialEq)]
pub struct Constructor {
    pub name: String,
    pub fields: Vec<Type>,
}

/// Complete program
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub declarations: Vec<Decl>,
    pub main_expr: Option<SpannedExpr>,
}

// Display implementations for pretty printing

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Lit(lit) => write!(f, "{}", lit),
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Lambda { param, body } => write!(f, "λ{} -> {}", param, body.node),
            Expr::Apply { func, arg } => write!(f, "({} {})", func.node, arg.node),
            Expr::BinOp { op, lhs, rhs } => write!(f, "({} {} {})", lhs.node, op, rhs.node),
            Expr::UnaryOp { op, operand } => write!(f, "{}{}", op, operand.node),
            Expr::Let { name, value, body } => {
                write!(f, "let {} := {} in {}", name, value.node, body.node)
            }
            Expr::Mu { name, body } => write!(f, "μ{} -> {}", name, body.node),
            Expr::Case {
                scrutinee,
                branches,
            } => {
                write!(f, "case {} of ", scrutinee.node)?;
                for (pat, expr) in branches {
                    write!(f, "{} => {}; ", pat.node, expr.node)?;
                }
                Ok(())
            }
            Expr::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(
                    f,
                    "if {} then {} else {}",
                    condition.node, then_branch.node, else_branch.node
                )
            }
            Expr::List(elements) => {
                write!(f, "[")?;
                for (i, elem) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem.node)?;
                }
                write!(f, "]")
            }
            Expr::Cons { head, tail } => write!(f, "({} : {})", head.node, tail.node),
            Expr::Block(exprs) => {
                write!(f, "{{ ")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}", expr.node)?;
                }
                write!(f, " }}")
            }
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int(n) => write!(f, "{}", n),
            Literal::Float(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Unit => write!(f, "()"),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Pow => "**",
            BinOp::Eq => "==",
            BinOp::Neq => "!=",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Gt => ">",
            BinOp::Ge => ">=",
            BinOp::And => "&&",
            BinOp::Or => "||",
            BinOp::Append => "<>",
            BinOp::Pipe => "|>",
        };
        write!(f, "{}", symbol)
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
        };
        write!(f, "{}", symbol)
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Wild => write!(f, "_"),
            Pattern::Var(name) => write!(f, "{}", name),
            Pattern::Lit(lit) => write!(f, "{}", lit),
            Pattern::List(patterns) => {
                write!(f, "[")?;
                for (i, pat) in patterns.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", pat.node)?;
                }
                write!(f, "]")
            }
            Pattern::Cons { head, tail } => write!(f, "({} : {})", head.node, tail.node),
            Pattern::Constructor { name, args } => {
                write!(f, "{}", name)?;
                if !args.is_empty() {
                    write!(f, "(")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg.node)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Pattern::Or(patterns) => {
                for (i, pat) in patterns.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", pat.node)?;
                }
                Ok(())
            }
            Pattern::Guard { pattern, condition } => {
                write!(f, "{} if {}", pattern.node, condition.node)
            }
        }
    }
}
