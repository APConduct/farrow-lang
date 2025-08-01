# Farrow Language Specification (Rust Implementation)

A purely functional, symbolic language with terse syntax and arrow-centric design.

## Syntax Cheat Sheet

### Core Operators
| Symbol | Purpose                  | Example                     |
|--------|--------------------------|-----------------------------|
| `:=`   | Definition               | `factorial := μ f n → ...`  |
| `→`    | Pipe/apply               | `x → f → g`                 |
| `↦`    | Lambda                   | `x ↦ x + 1`                 |
| `⇒`    | Type/pattern delimiter   | `Int ⇒ Int`, `case x of 0 ⇒ 1` |
| `μ`    | Recursion                | `μ f n → ...`               |

### Types (PascalCase)
```haskell
-- ADTs
type Maybe a = Nothing | Just a

-- Function type
map : (a ⇒ b) ⇒ List a ⇒ List b
```

### Functions
```haskell
-- Top-level
sum := μ f xs → case xs of
  Nil ⇒ 0
  (Cons x xs) ⇒ x + f xs

-- Lambda
xs → map (x ↦ x * 2) xs
```

### Pattern Matching
```haskell
case xs of
  Nil ⇒ 0
  (Cons x xs) ⇒ x + sum xs
```

## Semantics
- **Pure**: No side effects.
- **Lazy Evaluation**: Arguments evaluated only when needed.
- **Currying**: All functions take one argument.

<!--## Usage (Rust CLI)

### Build/Run
```bash
cargo build
./target/debug/farrow run example.fro
```-->

<!--### REPL
```bash
./target/debug/farrow repl
> 5 → (x ↦ x * 2) → print
10
```-->

## Examples

### Factorial
```haskell
factorial := μ f n →
  case n of
    0 ⇒ 1
    _ ⇒ n * f (n - 1)
```

### QuickSort
```haskell
sort := μ f xs →
  case xs of
    Nil ⇒ Nil
    (Cons p ps) ⇒
      (ps → filter (x ↦ x < p) → f
      → append (Cons p Nil)
      → (ps → filter (x ↦ x ≥ p) → f
```

<!--## Compiler Flags
- `--opt` : Enable optimizations
- `--lint` : Static checks-->
