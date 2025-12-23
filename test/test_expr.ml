(** Юнит-тесты для парсеров арифметических выражений *)

open Alcotest
open Parser_combinator.Expr_parser

let int_option = option int

(** {1 Тесты парсера выражений (со скобками)} *)

let test_expr_const () =
  let result = parse "123" in
  check
    (option (testable (fun fmt e -> Format.fprintf fmt "%s" (string_of_expr e)) ( = )))
    "parse constant" (Some (ConstExpr 123)) result

let test_expr_neg () =
  let result = parse "-42" in
  check
    (option (testable (fun fmt e -> Format.fprintf fmt "%s" (string_of_expr e)) ( = )))
    "parse negation" (Some (NegateExpr (ConstExpr 42))) result

let test_expr_add () =
  let result = parse "(1 + 2)" in
  let expected = Some (BinaryExpr (ConstExpr 1, Add, ConstExpr 2)) in
  check
    (option (testable (fun fmt e -> Format.fprintf fmt "%s" (string_of_expr e)) ( = )))
    "parse addition" expected result

let test_expr_mul () =
  let result = parse "(3 * 4)" in
  let expected = Some (BinaryExpr (ConstExpr 3, Mul, ConstExpr 4)) in
  check
    (option (testable (fun fmt e -> Format.fprintf fmt "%s" (string_of_expr e)) ( = )))
    "parse multiplication" expected result

let test_expr_nested () =
  let result = parse "(1 + ((2 + 3) * (4 + 5)))" in
  let expected =
    Some
      (BinaryExpr
         ( ConstExpr 1,
           Add,
           BinaryExpr
             ( BinaryExpr (ConstExpr 2, Add, ConstExpr 3),
               Mul,
               BinaryExpr (ConstExpr 4, Add, ConstExpr 5) ) ))
  in
  check
    (option (testable (fun fmt e -> Format.fprintf fmt "%s" (string_of_expr e)) ( = )))
    "parse nested expression" expected result

let test_expr_double_neg () =
  let result = parse "--5" in
  let expected = Some (NegateExpr (NegateExpr (ConstExpr 5))) in
  check
    (option (testable (fun fmt e -> Format.fprintf fmt "%s" (string_of_expr e)) ( = )))
    "parse double negation" expected result

let test_expr_neg_binary () =
  let result = parse "-(10 + 42)" in
  let expected = Some (NegateExpr (BinaryExpr (ConstExpr 10, Add, ConstExpr 42))) in
  check
    (option (testable (fun fmt e -> Format.fprintf fmt "%s" (string_of_expr e)) ( = )))
    "parse negated binary" expected result

let test_eval_const () =
  let result = parse_and_eval "123" in
  check int_option "eval constant" (Some 123) result

let test_eval_neg () =
  let result = parse_and_eval "-42" in
  check int_option "eval negation" (Some (-42)) result

let test_eval_add () =
  let result = parse_and_eval "(10 + 32)" in
  check int_option "eval addition" (Some 42) result

let test_eval_mul () =
  let result = parse_and_eval "(6 * 7)" in
  check int_option "eval multiplication" (Some 42) result

let test_eval_nested () =
  let result = parse_and_eval "(1 + ((2 + 3) * (4 + 5)))" in
  check int_option "eval nested" (Some 46) result

let test_invalid_expr_spaces () =
  let result = parse " 123 " in
  check
    (option (testable (fun fmt e -> Format.fprintf fmt "%s" (string_of_expr e)) ( = )))
    "reject leading/trailing spaces" None result

let test_invalid_expr_no_parens () =
  let result = parse "1 + 2" in
  check
    (option (testable (fun fmt e -> Format.fprintf fmt "%s" (string_of_expr e)) ( = )))
    "reject without parentheses" None result

let test_invalid_expr_double_space () =
  let result = parse "(1  + 2)" in
  check
    (option (testable (fun fmt e -> Format.fprintf fmt "%s" (string_of_expr e)) ( = )))
    "reject double space" None result

(** {1 Тесты инфиксного парсера (без обязательных скобок)} *)

open Parser_combinator.Infix_expr_parser

let test_infix_simple_add () =
  let result = parse "1 + 2" in
  check (option int) "1 + 2" (Some 3) (Option.map eval result)

let test_infix_precedence () =
  let result = parse "1 + 2 * 3" in
  check (option int) "1 + 2 * 3 = 7" (Some 7) (Option.map eval result)

let test_infix_precedence2 () =
  let result = parse "2 * 3 + 4" in
  check (option int) "2 * 3 + 4 = 10" (Some 10) (Option.map eval result)

let test_infix_parens () =
  let result = parse " ( 1 + 2 ) * 3 " in
  check (option int) "(1 + 2) * 3 = 9" (Some 9) (Option.map eval result)

let test_infix_neg () =
  let result = parse "-(5 + 3)" in
  check (option int) "-(5 + 3) = -8" (Some (-8)) (Option.map eval result)

let test_infix_division () =
  let result = parse "10 / 2 - 1" in
  check (option int) "10 / 2 - 1 = 4" (Some 4) (Option.map eval result)

let test_infix_complex () =
  let result = parse "10 + 2 * 5 - 3" in
  check (option int) "10 + 2 * 5 - 3 = 17" (Some 17) (Option.map eval result)

(** {1 Наборы тестов} *)

let expr_parser_tests =
  [
    test_case "parse constant" `Quick test_expr_const;
    test_case "parse negation" `Quick test_expr_neg;
    test_case "parse addition" `Quick test_expr_add;
    test_case "parse multiplication" `Quick test_expr_mul;
    test_case "parse nested" `Quick test_expr_nested;
    test_case "parse double negation" `Quick test_expr_double_neg;
    test_case "parse negated binary" `Quick test_expr_neg_binary;
    test_case "eval constant" `Quick test_eval_const;
    test_case "eval negation" `Quick test_eval_neg;
    test_case "eval addition" `Quick test_eval_add;
    test_case "eval multiplication" `Quick test_eval_mul;
    test_case "eval nested" `Quick test_eval_nested;
    test_case "invalid: leading/trailing spaces" `Quick test_invalid_expr_spaces;
    test_case "invalid: no parentheses" `Quick test_invalid_expr_no_parens;
    test_case "invalid: double space" `Quick test_invalid_expr_double_space;
  ]

let infix_expr_tests =
  [
    test_case "simple addition" `Quick test_infix_simple_add;
    test_case "precedence: 1+2*3" `Quick test_infix_precedence;
    test_case "precedence: 2*3+4" `Quick test_infix_precedence2;
    test_case "parentheses override" `Quick test_infix_parens;
    test_case "negation" `Quick test_infix_neg;
    test_case "division" `Quick test_infix_division;
    test_case "complex expression" `Quick test_infix_complex;
  ]

let () =
  run "Expression Parsers"
    [
      ("Expression Parser (with parens)", expr_parser_tests);
      ("Infix Expression Parser", infix_expr_tests);
    ]
