# Farrow Programming Language

A purely functional programming language with pattern matching, recursion, and arrow-centric design.

## Features Implemented

### ✅ Core Language Features
- **Pattern Matching**: Comprehensive pattern matching with wildcards, literals, lists, and cons patterns
- **Recursion**: μ (mu) operator for recursive functions
- **Lambda Functions**: First-class functions with `|->` syntax
- **List Operations**: Cons operator `:` and list literals `[1, 2, 3]`
- **Let Bindings**: Local variable bindings with `let x := value in expr`
- **Case Expressions**: Pattern matching with `case expr of pattern => result`

### ✅ Data Types
- **Integers**: `42`, `-10`
- **Strings**: `"Hello, World!"`
- **Booleans**: `true`, `false`
- **Lists**: `[1, 2, 3]`, `1 : [2, 3]`
- **Functions**: `x |-> x + 1`

### ✅ Operators
- **Arithmetic**: `+`, `-`, `*`, `/`, `%`
- **Comparison**: `==`, `!=`, `<`, `<=`, `>`, `>=`
- **Logical**: `&&`, `||`
- **List**: `:` (cons), `|>` (pipe)

## Syntax Examples

### Basic Expressions
```farrow
-- Numbers and arithmetic
42
1 + 2 * 3
5 - 2

-- Strings
"Hello, Farrow!"

-- Booleans
true && false
10 > 5
```

### Pattern Matching
```farrow
-- Simple pattern matching
case 42 of
  42 => "found forty-two"
  _ => "something else"

-- List pattern matching
case [1, 2, 3] of
  [] => "empty"
  h : t => h
  
-- Multiple patterns
case xs of
  [] => 0
  h : [] => h
  h : t => h + 10
```

### Functions and Recursion
```farrow
-- Lambda functions
increment := x |-> x + 1
double := x |-> x * 2

-- Recursive functions with μ
countdown := μf |-> (n |->
  case n of
    0 => "done"
    _ => f (n - 1))

-- Function application
increment 5
double (increment 3)
```

### Higher-Order Functions
```farrow
-- Function that takes another function
apply_twice := f |-> (x |-> f (f x))

-- Curried functions
adder := x |-> (y |-> x + y)
add_five := adder 5
```

### Let Bindings
```farrow
-- Simple let
let x := 5 in x + 3

-- Nested lets
let x := 5 in
  let y := x * 2 in
    x + y
```

### Lists and Cons
```farrow
-- List literals
[1, 2, 3, 4]

-- Cons operator
1 : [2, 3]
1 : 2 : [3, 4]

-- Building lists
let head := 42 in
let tail := [1, 2, 3] in
  head : tail
```

## Getting Started

### Building
```bash
cd farrow
cargo build
```

### Running
```bash
cargo run
```

This will run a series of test expressions demonstrating the language features.

### Examples
Check out the `examples/` directory for more comprehensive examples:
- `examples/pattern_matching.fro` - Pattern matching and basic recursion
- `examples/functions.fro` - Function composition and higher-order functions

## Language Specification

See `docs/SPEC.md` for the complete language specification.

## Implementation Status

### Core Features
- ✅ Lexical analysis (Logos)
- ✅ Recursive descent parser
- ✅ AST representation
- ✅ Basic interpreter with pattern matching
- ✅ Function closures and recursion
- ✅ List operations

### Future Features
- 🔄 Type system
- 🔄 Module system
- 🔄 ADTs (Algebraic Data Types)
- 🔄 Compilation to native code
- 🔄 REPL with readline support
- 🔄 Standard library

## Architecture

The language is implemented in Rust with the following components:

- **Lexer** (`src/lexer.rs`): Tokenization using Logos
- **Parser** (`src/parser.rs`): Hand-written recursive descent parser
- **AST** (`src/ast.rs`): Abstract syntax tree definitions
- **Evaluator** (`src/main.rs`): Tree-walking interpreter with pattern matching

## Contributing

This is a learning project implementing a functional programming language. Feel free to explore the code and suggest improvements!

## License

Apache-2.0