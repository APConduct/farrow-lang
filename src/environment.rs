use crate::error::{RuntimeError, RuntimeResult};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// A value that can be stored in the environment
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Unit,
    List(Vec<Value>),
    Function {
        param: String,
        body: crate::ast::SpannedExpr,
        env: Environment,
    },
    RecFunction {
        name: String,
        param: String,
        body: crate::ast::SpannedExpr,
        env: Environment,
    },
    BuiltinFunction(String),
    BuiltinFunction2(String, Box<Value>), // For curried built-ins
    Constructor {
        constructor: String,
        fields: Vec<Value>,
    },
}

impl Value {
    /// Get a human-readable type name for this value
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) => "integer",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Bool(_) => "boolean",
            Value::Unit => "unit",
            Value::List(_) => "list",
            Value::Function { .. } => "function",
            Value::RecFunction { .. } => "recursive function",
            Value::BuiltinFunction(_) => "builtin function",
            Value::BuiltinFunction2(..) => "builtin function",
            Value::Constructor { .. } => "constructor",
        }
    }

    /// Check if this value is callable (a function)
    pub fn is_callable(&self) -> bool {
        matches!(
            self,
            Value::Function { .. }
                | Value::RecFunction { .. }
                | Value::BuiltinFunction(_)
                | Value::BuiltinFunction2(..)
        )
    }

    /// Check if this value is truthy (for conditional expressions)
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Unit => false,
            Value::Int(0) => false,
            Value::Float(f) => *f != 0.0,
            Value::List(list) => !list.is_empty(),
            Value::String(s) => !s.is_empty(),
            _ => true,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Unit => write!(f, "()"),
            Value::List(values) => {
                write!(f, "[")?;
                for (i, val) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, "]")
            }
            Value::Function { param, .. } => write!(f, "<function {}>", param),
            Value::RecFunction { name, param, .. } => {
                write!(f, "<recursive function {} {}>", name, param)
            }
            Value::BuiltinFunction(name) => write!(f, "<builtin {}>", name),
            Value::BuiltinFunction2(name, _) => write!(f, "<builtin {} (partial)>", name),
            Value::Constructor {
                constructor,
                fields,
            } => {
                if fields.is_empty() {
                    write!(f, "{}", constructor)
                } else {
                    write!(f, "({}", constructor)?;
                    for field in fields {
                        write!(f, " {}", field)?;
                    }
                    write!(f, ")")
                }
            }
        }
    }
}

/// Environment for variable bindings with lexical scoping
#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    bindings: Rc<RefCell<HashMap<String, Value>>>,
    parent: Option<Rc<Environment>>,
}

impl Environment {
    /// Create a new empty environment
    pub fn new() -> Self {
        Environment {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            parent: None,
        }
    }

    /// Create a new environment with a parent
    pub fn with_parent(parent: Environment) -> Self {
        Environment {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            parent: Some(Rc::new(parent)),
        }
    }

    /// Create a child environment
    pub fn extend(&self) -> Environment {
        Environment {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            parent: Some(Rc::new(self.clone())),
        }
    }

    /// Define a new binding in this environment
    pub fn define(&self, name: String, value: Value) {
        self.bindings.borrow_mut().insert(name, value);
    }

    /// Look up a variable in this environment or its parents
    pub fn lookup(&self, name: &str) -> RuntimeResult<Value> {
        // Check current environment
        if let Some(value) = self.bindings.borrow().get(name) {
            return Ok(value.clone());
        }

        // Check parent environments
        if let Some(parent) = &self.parent {
            return parent.lookup(name);
        }

        Err(RuntimeError::unbound_variable(name.to_string()))
    }

    /// Update an existing binding (for mutation, if we add it later)
    pub fn update(&self, name: &str, value: Value) -> RuntimeResult<()> {
        // Check current environment
        if self.bindings.borrow().contains_key(name) {
            self.bindings.borrow_mut().insert(name.to_string(), value);
            return Ok(());
        }

        // Check parent environments
        if let Some(parent) = &self.parent {
            return parent.update(name, value);
        }

        Err(RuntimeError::unbound_variable(name.to_string()))
    }

    /// Check if a variable is defined in this environment or its parents
    pub fn contains(&self, name: &str) -> bool {
        self.bindings.borrow().contains_key(name)
            || self.parent.as_ref().map_or(false, |p| p.contains(name))
    }

    /// Get all bindings in this environment (not including parents)
    pub fn local_bindings(&self) -> HashMap<String, Value> {
        self.bindings.borrow().clone()
    }

    /// Get the depth of this environment (for debugging)
    pub fn depth(&self) -> usize {
        match &self.parent {
            Some(parent) => 1 + parent.depth(),
            None => 0,
        }
    }

    /// Create a global environment with standard library functions
    pub fn global() -> Self {
        let env = Environment::new();

        // I/O functions
        env.define(
            "print".to_string(),
            Value::BuiltinFunction("print".to_string()),
        );
        env.define(
            "println".to_string(),
            Value::BuiltinFunction("println".to_string()),
        );
        env.define(
            "print_line".to_string(),
            Value::BuiltinFunction("print_line".to_string()),
        );

        // List functions - basic
        env.define(
            "length".to_string(),
            Value::BuiltinFunction("length".to_string()),
        );
        env.define(
            "head".to_string(),
            Value::BuiltinFunction("head".to_string()),
        );
        env.define(
            "tail".to_string(),
            Value::BuiltinFunction("tail".to_string()),
        );
        env.define(
            "empty?".to_string(),
            Value::BuiltinFunction("empty?".to_string()),
        );
        env.define(
            "cons".to_string(),
            Value::BuiltinFunction("cons".to_string()),
        );
        env.define(
            "append".to_string(),
            Value::BuiltinFunction("append".to_string()),
        );
        env.define(
            "reverse".to_string(),
            Value::BuiltinFunction("reverse".to_string()),
        );

        // List functions - advanced
        env.define("map".to_string(), Value::BuiltinFunction("map".to_string()));
        env.define(
            "filter".to_string(),
            Value::BuiltinFunction("filter".to_string()),
        );
        env.define(
            "fold".to_string(),
            Value::BuiltinFunction("fold".to_string()),
        );
        env.define(
            "reduce".to_string(),
            Value::BuiltinFunction("reduce".to_string()),
        );
        env.define("zip".to_string(), Value::BuiltinFunction("zip".to_string()));
        env.define(
            "take".to_string(),
            Value::BuiltinFunction("take".to_string()),
        );
        env.define(
            "drop".to_string(),
            Value::BuiltinFunction("drop".to_string()),
        );
        env.define(
            "concat".to_string(),
            Value::BuiltinFunction("concat".to_string()),
        );
        env.define(
            "flatten".to_string(),
            Value::BuiltinFunction("flatten".to_string()),
        );
        env.define(
            "sort".to_string(),
            Value::BuiltinFunction("sort".to_string()),
        );
        env.define(
            "contains".to_string(),
            Value::BuiltinFunction("contains".to_string()),
        );
        env.define(
            "index_of".to_string(),
            Value::BuiltinFunction("index_of".to_string()),
        );

        // String functions
        env.define(
            "string_length".to_string(),
            Value::BuiltinFunction("string_length".to_string()),
        );
        env.define(
            "string_concat".to_string(),
            Value::BuiltinFunction("string_concat".to_string()),
        );
        env.define(
            "string_split".to_string(),
            Value::BuiltinFunction("string_split".to_string()),
        );
        env.define(
            "string_join".to_string(),
            Value::BuiltinFunction("string_join".to_string()),
        );
        env.define(
            "string_trim".to_string(),
            Value::BuiltinFunction("string_trim".to_string()),
        );
        env.define(
            "string_upper".to_string(),
            Value::BuiltinFunction("string_upper".to_string()),
        );
        env.define(
            "string_lower".to_string(),
            Value::BuiltinFunction("string_lower".to_string()),
        );
        env.define(
            "string_replace".to_string(),
            Value::BuiltinFunction("string_replace".to_string()),
        );
        env.define(
            "string_contains".to_string(),
            Value::BuiltinFunction("string_contains".to_string()),
        );
        env.define(
            "string_starts_with".to_string(),
            Value::BuiltinFunction("string_starts_with".to_string()),
        );
        env.define(
            "string_ends_with".to_string(),
            Value::BuiltinFunction("string_ends_with".to_string()),
        );
        env.define(
            "char_at".to_string(),
            Value::BuiltinFunction("char_at".to_string()),
        );

        // Math functions - basic
        env.define("abs".to_string(), Value::BuiltinFunction("abs".to_string()));
        env.define("min".to_string(), Value::BuiltinFunction("min".to_string()));
        env.define("max".to_string(), Value::BuiltinFunction("max".to_string()));
        env.define(
            "sign".to_string(),
            Value::BuiltinFunction("sign".to_string()),
        );

        // Math functions - advanced
        env.define(
            "sqrt".to_string(),
            Value::BuiltinFunction("sqrt".to_string()),
        );
        env.define("pow".to_string(), Value::BuiltinFunction("pow".to_string()));
        env.define("exp".to_string(), Value::BuiltinFunction("exp".to_string()));
        env.define("log".to_string(), Value::BuiltinFunction("log".to_string()));
        env.define("ln".to_string(), Value::BuiltinFunction("ln".to_string()));
        env.define("sin".to_string(), Value::BuiltinFunction("sin".to_string()));
        env.define("cos".to_string(), Value::BuiltinFunction("cos".to_string()));
        env.define("tan".to_string(), Value::BuiltinFunction("tan".to_string()));
        env.define(
            "asin".to_string(),
            Value::BuiltinFunction("asin".to_string()),
        );
        env.define(
            "acos".to_string(),
            Value::BuiltinFunction("acos".to_string()),
        );
        env.define(
            "atan".to_string(),
            Value::BuiltinFunction("atan".to_string()),
        );
        env.define(
            "floor".to_string(),
            Value::BuiltinFunction("floor".to_string()),
        );
        env.define(
            "ceil".to_string(),
            Value::BuiltinFunction("ceil".to_string()),
        );
        env.define(
            "round".to_string(),
            Value::BuiltinFunction("round".to_string()),
        );

        // Math constants
        env.define("pi".to_string(), Value::Float(std::f64::consts::PI));
        env.define("e".to_string(), Value::Float(std::f64::consts::E));

        // Type conversion functions
        env.define(
            "to_string".to_string(),
            Value::BuiltinFunction("to_string".to_string()),
        );
        env.define(
            "to_int".to_string(),
            Value::BuiltinFunction("to_int".to_string()),
        );
        env.define(
            "to_float".to_string(),
            Value::BuiltinFunction("to_float".to_string()),
        );

        // Type predicates
        env.define(
            "int?".to_string(),
            Value::BuiltinFunction("int?".to_string()),
        );
        env.define(
            "float?".to_string(),
            Value::BuiltinFunction("float?".to_string()),
        );
        env.define(
            "string?".to_string(),
            Value::BuiltinFunction("string?".to_string()),
        );
        env.define(
            "bool?".to_string(),
            Value::BuiltinFunction("bool?".to_string()),
        );
        env.define(
            "list?".to_string(),
            Value::BuiltinFunction("list?".to_string()),
        );
        env.define(
            "function?".to_string(),
            Value::BuiltinFunction("function?".to_string()),
        );
        env.define(
            "unit?".to_string(),
            Value::BuiltinFunction("unit?".to_string()),
        );

        // Utility functions
        env.define(
            "identity".to_string(),
            Value::BuiltinFunction("identity".to_string()),
        );
        env.define(
            "const".to_string(),
            Value::BuiltinFunction("const".to_string()),
        );
        env.define(
            "compose".to_string(),
            Value::BuiltinFunction("compose".to_string()),
        );
        env.define(
            "apply".to_string(),
            Value::BuiltinFunction("apply".to_string()),
        );

        env
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

/// Call frame for stack traces
#[derive(Debug, Clone)]
pub struct CallFrame {
    pub function_name: Option<String>,
    pub span: Option<crate::ast::Span>,
    pub env_depth: usize,
}

impl CallFrame {
    pub fn new(
        function_name: Option<String>,
        span: Option<crate::ast::Span>,
        env_depth: usize,
    ) -> Self {
        CallFrame {
            function_name,
            span,
            env_depth,
        }
    }

    pub fn anonymous(span: Option<crate::ast::Span>, env_depth: usize) -> Self {
        CallFrame::new(None, span, env_depth)
    }

    pub fn named(name: String, span: Option<crate::ast::Span>, env_depth: usize) -> Self {
        CallFrame::new(Some(name), span, env_depth)
    }
}

impl std::fmt::Display for CallFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.function_name {
            Some(name) => write!(f, "in function '{}'", name),
            None => write!(f, "in anonymous function"),
        }?;

        if let Some(span) = &self.span {
            write!(f, " at {}:{}", span.start, span.end)?;
        }

        Ok(())
    }
}

/// Call stack for tracking function calls and providing stack traces
#[derive(Debug, Clone)]
pub struct CallStack {
    frames: Vec<CallFrame>,
    max_depth: usize,
}

impl CallStack {
    /// Create a new call stack with a maximum depth
    pub fn new(max_depth: usize) -> Self {
        CallStack {
            frames: Vec::new(),
            max_depth,
        }
    }

    /// Push a new frame onto the stack
    pub fn push(&mut self, frame: CallFrame) -> RuntimeResult<()> {
        if self.frames.len() >= self.max_depth {
            return Err(RuntimeError::stack_overflow());
        }
        self.frames.push(frame);
        Ok(())
    }

    /// Pop the top frame from the stack
    pub fn pop(&mut self) -> Option<CallFrame> {
        self.frames.pop()
    }

    /// Get the current stack depth
    pub fn depth(&self) -> usize {
        self.frames.len()
    }

    /// Check if the stack is empty
    pub fn is_empty(&self) -> bool {
        self.frames.is_empty()
    }

    /// Get the top frame without removing it
    pub fn top(&self) -> Option<&CallFrame> {
        self.frames.last()
    }

    /// Get all frames for error reporting
    pub fn frames(&self) -> &[CallFrame] {
        &self.frames
    }

    /// Create a formatted stack trace
    pub fn stack_trace(&self) -> String {
        if self.frames.is_empty() {
            return "  (no stack trace available)".to_string();
        }

        let mut trace = String::new();
        for (i, frame) in self.frames.iter().rev().enumerate() {
            if i > 0 {
                trace.push('\n');
            }
            trace.push_str(&format!("  {}: {}", i, frame));
        }
        trace
    }
}

impl Default for CallStack {
    fn default() -> Self {
        Self::new(1000) // Default max depth of 1000
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_environment_basic() {
        let env = Environment::new();

        // Define a variable
        env.define("x".to_string(), Value::Int(42));

        // Look it up
        assert_eq!(env.lookup("x").unwrap(), Value::Int(42));

        // Look up non-existent variable
        assert!(env.lookup("y").is_err());
    }

    #[test]
    fn test_environment_scoping() {
        let parent = Environment::new();
        parent.define("x".to_string(), Value::Int(1));
        parent.define("y".to_string(), Value::Int(2));

        let child = Environment::with_parent(parent);
        child.define("y".to_string(), Value::Int(3)); // Shadow parent's y
        child.define("z".to_string(), Value::Int(4));

        // Child can see its own bindings
        assert_eq!(child.lookup("z").unwrap(), Value::Int(4));

        // Child sees shadowed variable
        assert_eq!(child.lookup("y").unwrap(), Value::Int(3));

        // Child can see parent's non-shadowed variable
        assert_eq!(child.lookup("x").unwrap(), Value::Int(1));
    }

    #[test]
    fn test_call_stack() {
        let mut stack = CallStack::new(3);

        assert!(stack.is_empty());
        assert_eq!(stack.depth(), 0);

        // Push frames
        stack
            .push(CallFrame::named("main".to_string(), None, 0))
            .unwrap();
        stack
            .push(CallFrame::named("foo".to_string(), None, 1))
            .unwrap();

        assert_eq!(stack.depth(), 2);
        assert_eq!(stack.top().unwrap().function_name, Some("foo".to_string()));

        // Test stack overflow
        stack
            .push(CallFrame::named("bar".to_string(), None, 2))
            .unwrap();
        assert!(stack
            .push(CallFrame::named("overflow".to_string(), None, 3))
            .is_err());
    }

    #[test]
    fn test_value_type_names() {
        assert_eq!(Value::Int(42).type_name(), "integer");
        assert_eq!(Value::String("hello".to_string()).type_name(), "string");
        assert_eq!(Value::Bool(true).type_name(), "boolean");
        assert_eq!(Value::List(vec![]).type_name(), "list");
    }

    #[test]
    fn test_value_truthiness() {
        assert!(Value::Bool(true).is_truthy());
        assert!(!Value::Bool(false).is_truthy());
        assert!(!Value::Int(0).is_truthy());
        assert!(Value::Int(1).is_truthy());
        assert!(!Value::List(vec![]).is_truthy());
        assert!(Value::List(vec![Value::Int(1)]).is_truthy());
    }
}
