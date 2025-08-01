use farrow::{environment::Environment, evaluator::Evaluator, parser::parse_expr_from_str};

fn eval_string(input: &str) -> Result<String, String> {
    let expr = parse_expr_from_str(input).map_err(|e| format!("Parse error: {}", e))?;
    let mut evaluator = Evaluator::new();
    let env = Environment::global();

    evaluator
        .eval(&env, &expr)
        .map(|v| v.to_string())
        .map_err(|e| format!("Runtime error: {}", e))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_literals() {
        assert_eq!(eval_string("42").unwrap(), "42");
        assert_eq!(eval_string("true").unwrap(), "true");
        assert_eq!(eval_string("false").unwrap(), "false");
        assert_eq!(eval_string(r#""hello""#).unwrap(), r#""hello""#);
    }

    #[test]
    fn test_arithmetic() {
        assert_eq!(eval_string("1 + 2").unwrap(), "3");
        assert_eq!(eval_string("5 - 3").unwrap(), "2");
        assert_eq!(eval_string("4 * 3").unwrap(), "12");
        assert_eq!(eval_string("10 / 2").unwrap(), "5");
        assert_eq!(eval_string("7 % 3").unwrap(), "1");

        // Test precedence
        assert_eq!(eval_string("1 + 2 * 3").unwrap(), "7");
        assert_eq!(eval_string("(1 + 2) * 3").unwrap(), "9");
    }

    #[test]
    fn test_comparison() {
        assert_eq!(eval_string("5 > 3").unwrap(), "true");
        assert_eq!(eval_string("3 < 5").unwrap(), "true");
        assert_eq!(eval_string("5 >= 5").unwrap(), "true");
        assert_eq!(eval_string("3 <= 5").unwrap(), "true");
        assert_eq!(eval_string("5 == 5").unwrap(), "true");
        assert_eq!(eval_string("5 != 3").unwrap(), "true");
    }

    #[test]
    fn test_logical_operators() {
        assert_eq!(eval_string("true && true").unwrap(), "true");
        assert_eq!(eval_string("true && false").unwrap(), "false");
        assert_eq!(eval_string("true || false").unwrap(), "true");
        assert_eq!(eval_string("false || false").unwrap(), "false");
    }

    #[test]
    fn test_lists() {
        assert_eq!(eval_string("[]").unwrap(), "[]");
        assert_eq!(eval_string("[1, 2, 3]").unwrap(), "[1, 2, 3]");
        assert_eq!(eval_string("1 : [2, 3]").unwrap(), "[1, 2, 3]");
        assert_eq!(eval_string("1 : 2 : [3]").unwrap(), "[1, 2, 3]");
    }

    #[test]
    fn test_lambda_functions() {
        assert_eq!(eval_string("(x |-> x + 1) 5").unwrap(), "6");
        assert_eq!(eval_string("(x |-> x * 2) 3").unwrap(), "6");

        // Test currying
        let result = eval_string("(x |-> (y |-> x + y)) 5 3");
        assert_eq!(result.unwrap(), "8");
    }

    #[test]
    fn test_let_bindings() {
        assert_eq!(eval_string("let x := 5 in x + 3").unwrap(), "8");

        // Nested lets
        let nested = "let x := 5 in let y := x * 2 in x + y";
        assert_eq!(eval_string(nested).unwrap(), "15");

        // Let with functions
        let func_let = "let f := x |-> x * 2 in f 7";
        assert_eq!(eval_string(func_let).unwrap(), "14");
    }

    #[test]
    fn test_pattern_matching() {
        // Simple literal patterns
        assert_eq!(
            eval_string("case 42 of 42 => true, _ => false").unwrap(),
            "true"
        );
        assert_eq!(
            eval_string("case 99 of 42 => true, _ => false").unwrap(),
            "false"
        );

        // Boolean patterns
        assert_eq!(
            eval_string("case true of true => 1, false => 0").unwrap(),
            "1"
        );

        // List patterns
        assert_eq!(eval_string("case [] of [] => 0, _ => 1").unwrap(), "0");
        assert_eq!(
            eval_string("case [1, 2, 3] of [] => 0, h : t => h").unwrap(),
            "1"
        );

        // Variable patterns
        assert_eq!(eval_string("case 42 of x => x + 1").unwrap(), "43");
    }

    #[test]
    fn test_recursive_functions() {
        // Simple recursive function
        let countdown = r#"
            let countdown := μf |-> (n |->
                case n of
                    0 => 0,
                    _ => f (n - 1))
            in countdown 3
        "#;
        assert_eq!(eval_string(countdown).unwrap(), "0");

        // Recursive function with accumulation
        let sum_to_n = r#"
            let sum := μf |-> (n |->
                case n of
                    0 => 0,
                    _ => n + f (n - 1))
            in sum 5
        "#;
        assert_eq!(eval_string(sum_to_n).unwrap(), "15");
    }

    #[test]
    fn test_pipe_operator() {
        assert_eq!(eval_string("5 |> (x |-> x + 1)").unwrap(), "6");
        assert_eq!(
            eval_string("10 |> (x |-> x * 2) |> (x |-> x + 1)").unwrap(),
            "21"
        );
    }

    #[test]
    fn test_builtin_functions() {
        // List functions
        assert_eq!(eval_string("length [1, 2, 3, 4]").unwrap(), "4");
        assert_eq!(eval_string("head [10, 20, 30]").unwrap(), "10");
        assert_eq!(eval_string("tail [10, 20, 30]").unwrap(), "[20, 30]");
        assert_eq!(eval_string("empty? []").unwrap(), "true");
        assert_eq!(eval_string("empty? [1]").unwrap(), "false");

        // Math functions
        assert_eq!(eval_string("abs (-5)").unwrap(), "5");
        assert_eq!(eval_string("abs 5").unwrap(), "5");

        // Type predicates
        assert_eq!(eval_string("int? 42").unwrap(), "true");
        assert_eq!(eval_string("int? \"hello\"").unwrap(), "false");
        assert_eq!(eval_string("string? \"hello\"").unwrap(), "true");
        assert_eq!(eval_string("bool? true").unwrap(), "true");
        assert_eq!(eval_string("list? [1, 2, 3]").unwrap(), "true");
    }

    #[test]
    fn test_complex_expressions() {
        // Function composition - test individual parts first
        assert_eq!(eval_string("let inc := x |-> x + 1 in inc 5").unwrap(), "6");
        assert_eq!(
            eval_string("let double := x |-> x * 2 in double 3").unwrap(),
            "6"
        );

        // Simple higher-order function
        let apply_twice = "let apply_twice := f |-> (x |-> f (f x)) in apply_twice";
        assert!(eval_string(apply_twice).is_ok());
    }

    #[test]
    fn test_error_cases() {
        // Unbound variable
        assert!(eval_string("undefined_var").is_err());

        // Division by zero
        assert!(eval_string("1 / 0").is_err());

        // Empty list operations
        assert!(eval_string("head []").is_err());
        assert!(eval_string("tail []").is_err());

        // Type mismatches
        assert!(eval_string("1 + true").is_err());
        assert!(eval_string("\"hello\" * 5").is_err());

        // Pattern match failure
        assert!(eval_string("case 5 of 1 => true, 2 => false").is_err());

        // Invalid function application
        assert!(eval_string("42 5").is_err());
    }

    #[test]
    fn test_scoping() {
        // Variable shadowing
        let shadow = r#"
            let x := 5 in
            let x := 10 in
            x
        "#;
        assert_eq!(eval_string(shadow).unwrap(), "10");

        // Closure captures environment
        let closure = r#"
            let x := 5 in
            let f := y |-> x + y in
            let x := 10 in
            f 3
        "#;
        assert_eq!(eval_string(closure).unwrap(), "8");

        // Function parameter shadowing
        let param_shadow = r#"
            let x := 5 in
            (x |-> x + 1) 10
        "#;
        assert_eq!(eval_string(param_shadow).unwrap(), "11");
    }

    #[test]
    fn test_edge_cases() {
        // Empty expressions
        assert_eq!(eval_string("()").unwrap(), "()");

        // Nested parentheses
        assert_eq!(eval_string("((((5))))").unwrap(), "5");

        // Complex arithmetic
        assert_eq!(eval_string("1 + 2 * 3 + 4 * 5").unwrap(), "27");

        // Deeply nested lists
        assert_eq!(eval_string("1 : 2 : 3 : []").unwrap(), "[1, 2, 3]");

        // Function returning function
        let curried = "(x |-> (y |-> (z |-> x + y + z))) 1 2 3";
        assert_eq!(eval_string(curried).unwrap(), "6");
    }

    #[test]
    fn test_string_operations() {
        // String concatenation
        assert_eq!(
            eval_string(r#""hello" + " " + "world""#).unwrap(),
            r#""hello world""#
        );

        // String length
        assert_eq!(eval_string(r#"length "hello""#).unwrap(), "5");

        // Empty string
        assert_eq!(eval_string(r#""""#).unwrap(), r#""""#);
    }

    #[test]
    fn test_boolean_short_circuiting() {
        // These should not cause division by zero due to short-circuiting
        assert_eq!(eval_string("false && (1 / 0 > 0)").unwrap(), "false");
        assert_eq!(eval_string("true || (1 / 0 > 0)").unwrap(), "true");
    }

    #[test]
    fn test_block_expressions() {
        // Empty block
        assert_eq!(eval_string("{}").unwrap(), "()");

        // Single expression block
        assert_eq!(eval_string("{ 42 }").unwrap(), "42");

        // Multiple expressions - returns last value
        assert_eq!(eval_string("{ 1; 2; 3 }").unwrap(), "3");

        // Block with semicolon after last expression
        assert_eq!(eval_string("{ 1; 2; 3; }").unwrap(), "3");

        // Block with let bindings
        let block_with_let = "{ let x := 5 in let y := x * 2 in x + y }";
        assert_eq!(eval_string(block_with_let).unwrap(), "15");

        // Nested blocks
        let nested = "{ let x := 5 in { let y := x * 2 in y + 3 } }";
        assert_eq!(eval_string(nested).unwrap(), "13");

        // Block as function argument
        let block_arg = "(f |-> f 10) (x |-> { let y := 5 in x + y })";
        assert_eq!(eval_string(block_arg).unwrap(), "15");

        // Block with function definitions
        let block_func = "{ let inc := x |-> x + 1 in inc 5 }";
        assert_eq!(eval_string(block_func).unwrap(), "6");
    }

    #[test]
    fn test_block_scoping() {
        // Variables in blocks don't leak out
        let scoped = r#"
            let x := 10 in
            let result := { let x := 5; x + 1 } in
            x + result
        "#;
        assert_eq!(eval_string(scoped).unwrap(), "16");

        // Block bindings are visible in later expressions
        let sequential = "{ let x := 5 in let y := x * 2 in let z := y + 3 in z }";
        assert_eq!(eval_string(sequential).unwrap(), "13");

        // Shadowing in blocks
        let shadowing = r#"
            let x := 1 in
            { let x := 2; { let x := 3; x } + x } + x
        "#;
        assert_eq!(eval_string(shadowing).unwrap(), "6"); // 3 + 2 + 1
    }

    #[test]
    fn test_block_with_pattern_matching() {
        let block_case = r#"
            {
                let xs := [1, 2, 3] in
                case xs of
                    [] => 0,
                    h : t => h + length t
            }
        "#;
        assert_eq!(eval_string(block_case).unwrap(), "3");
    }

    #[test]
    fn test_block_with_recursive_functions() {
        let block_recursive = r#"
            {
                let factorial := μf |-> (n |->
                    case n of
                        0 => 1,
                        _ => n * f (n - 1)) in
                factorial 4
            }
        "#;
        assert_eq!(eval_string(block_recursive).unwrap(), "24");
    }

    #[test]
    fn test_enhanced_standard_library() {
        // Higher-order list functions
        assert_eq!(
            eval_string("map (x |-> x * 2) [1, 2, 3]").unwrap(),
            "[2, 4, 6]"
        );
        assert_eq!(
            eval_string("filter (x |-> x > 2) [1, 2, 3, 4, 5]").unwrap(),
            "[3, 4, 5]"
        );

        // List utility functions
        assert_eq!(eval_string("take 3 [1, 2, 3, 4, 5]").unwrap(), "[1, 2, 3]");
        assert_eq!(eval_string("drop 2 [1, 2, 3, 4, 5]").unwrap(), "[3, 4, 5]");
        assert_eq!(eval_string("concat [1, 2] [3, 4]").unwrap(), "[1, 2, 3, 4]");
        assert_eq!(
            eval_string("sort [3, 1, 4, 1, 5]").unwrap(),
            "[1, 1, 3, 4, 5]"
        );
        assert_eq!(
            eval_string("flatten [[1, 2], [3, 4]]").unwrap(),
            "[1, 2, 3, 4]"
        );
        assert_eq!(eval_string("contains 3 [1, 2, 3, 4]").unwrap(), "true");
        assert_eq!(eval_string("contains 9 [1, 2, 3, 4]").unwrap(), "false");

        // String functions
        assert_eq!(eval_string("string_length \"hello\"").unwrap(), "5");
        assert_eq!(
            eval_string("string_concat \"hello\" \" world\"").unwrap(),
            "\"hello world\""
        );
        assert_eq!(
            eval_string("string_trim \"  hello  \"").unwrap(),
            "\"hello\""
        );
        assert_eq!(eval_string("string_upper \"hello\"").unwrap(), "\"HELLO\"");
        assert_eq!(eval_string("string_lower \"HELLO\"").unwrap(), "\"hello\"");
        assert_eq!(
            eval_string("string_contains \"ell\" \"hello\"").unwrap(),
            "true"
        );
        assert_eq!(
            eval_string("string_starts_with \"he\" \"hello\"").unwrap(),
            "true"
        );
        assert_eq!(
            eval_string("string_ends_with \"lo\" \"hello\"").unwrap(),
            "true"
        );
        assert_eq!(eval_string("char_at 1 \"hello\"").unwrap(), "\"e\"");
        assert_eq!(
            eval_string("string_split \" \" \"hello world\"").unwrap(),
            "[\"hello\", \"world\"]"
        );

        // Math functions
        assert_eq!(eval_string("min 5 3").unwrap(), "3");
        assert_eq!(eval_string("max 5 3").unwrap(), "5");
        assert_eq!(eval_string("pow 2 8").unwrap(), "256");
        assert_eq!(eval_string("sqrt 16").unwrap(), "4");
        assert_eq!(eval_string("abs (-5)").unwrap(), "5");
        assert_eq!(eval_string("sign (-5)").unwrap(), "-1");
        assert_eq!(eval_string("sign 5").unwrap(), "1");
        assert_eq!(eval_string("floor 3.7").unwrap(), "3");
        assert_eq!(eval_string("ceil 3.2").unwrap(), "4");
        assert_eq!(eval_string("round 3.6").unwrap(), "4");

        // Trigonometric functions (approximate tests)
        let sin_result = eval_string("sin 0").unwrap();
        assert_eq!(sin_result, "0");
        let cos_result = eval_string("cos 0").unwrap();
        assert_eq!(cos_result, "1");

        // Type conversion functions
        assert_eq!(eval_string("to_string 42").unwrap(), "\"42\"");
        assert_eq!(eval_string("to_int \"42\"").unwrap(), "42");
        assert_eq!(eval_string("to_float 42").unwrap(), "42");

        // Extended type predicates
        assert_eq!(eval_string("int? 42").unwrap(), "true");
        assert_eq!(eval_string("float? 3.14").unwrap(), "true");
        assert_eq!(eval_string("string? \"hello\"").unwrap(), "true");
        assert_eq!(eval_string("bool? true").unwrap(), "true");
        assert_eq!(eval_string("list? [1, 2, 3]").unwrap(), "true");
        assert_eq!(eval_string("unit? ()").unwrap(), "true");

        // Utility functions
        assert_eq!(eval_string("identity 42").unwrap(), "42");
        assert_eq!(eval_string("const 42 99").unwrap(), "42");
        assert_eq!(eval_string("apply (x |-> x + 1) 5").unwrap(), "6");
    }

    #[test]
    fn test_standard_library_composition() {
        // Test composing multiple standard library functions
        let complex_example = r#"
            let numbers := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] in
            let evens := filter (x |-> x % 2 == 0) numbers in
            let doubled := map (x |-> x * 2) evens in
            take 3 doubled
        "#;
        assert_eq!(eval_string(complex_example).unwrap(), "[4, 8, 12]");

        // String processing pipeline
        let string_pipeline = r#"
            let text := "  Hello World  " in
            let trimmed := string_trim text in
            let upper := string_upper trimmed in
            string_split " " upper
        "#;
        assert_eq!(
            eval_string(string_pipeline).unwrap(),
            "[\"HELLO\", \"WORLD\"]"
        );

        // Math computation
        let math_computation = r#"
            let numbers := [1, 4, 9, 16] in
            let roots := map sqrt numbers in
            map (x |-> round x) roots
        "#;
        assert_eq!(eval_string(math_computation).unwrap(), "[1, 2, 3, 4]");
    }

    #[test]
    fn test_curried_functions() {
        // Test that multi-argument functions are properly curried
        assert_eq!(eval_string("let add := pow 2 in add 3").unwrap(), "8");
        assert_eq!(
            eval_string("let double_all := map (x |-> x * 2) in double_all [1, 2, 3]").unwrap(),
            "[2, 4, 6]"
        );
        assert_eq!(
            eval_string("let take_two := take 2 in take_two [1, 2, 3, 4]").unwrap(),
            "[1, 2]"
        );
    }

    #[test]
    fn test_standard_library_error_cases() {
        // Test error handling in standard library functions
        assert!(eval_string("char_at 10 \"hello\"").is_err()); // Index out of bounds
        assert!(eval_string("to_int \"not_a_number\"").is_err()); // Invalid conversion
        assert!(eval_string("to_float \"invalid\"").is_err()); // Invalid conversion
        assert!(eval_string("sqrt (-1)").is_ok()); // Should return NaN but not error
    }
}
