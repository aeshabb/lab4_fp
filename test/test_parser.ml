(** Unit tests for the Parser Combinator library *)

open Alcotest
open Parser_combinator.Parser
open Parser_combinator.Expr_parser

(** {1 Test Helpers} *)

let pp_string_pair fmt (s, v) = Format.fprintf fmt "(%S, %s)" s v
let pp_int_pair fmt (s, v) = Format.fprintf fmt "(%S, %d)" s v
let pp_char_pair fmt (s, v) = Format.fprintf fmt "(%S, %c)" s v
let pp_unit_pair fmt (s, ()) = Format.fprintf fmt "(%S, ())" s
let string_pair_list = list (testable pp_string_pair ( = ))
let int_pair_list = list (testable pp_int_pair ( = ))
let char_pair_list = list (testable pp_char_pair ( = ))
let unit_pair_list = list (testable pp_unit_pair ( = ))
let int_option = option int

(** {1 Basic Parser Tests} *)

let test_satisfy_success () =
  let p = satisfy (fun c -> c = 'a') in
  let result = run_parser p "abc" in
  check char_pair_list "satisfy matches" [ ("bc", 'a') ] result

let test_satisfy_failure () =
  let p = satisfy (fun c -> c = 'a') in
  let result = run_parser p "bcd" in
  check char_pair_list "satisfy fails" [] result

let test_satisfy_empty () =
  let p = satisfy (fun c -> c = 'a') in
  let result = run_parser p "" in
  check char_pair_list "satisfy on empty string" [] result

let test_char_p_success () =
  let result = run_parser (char_p 'x') "xyz" in
  check char_pair_list "char_p matches" [ ("yz", 'x') ] result

let test_char_p_failure () =
  let result = run_parser (char_p 'x') "abc" in
  check char_pair_list "char_p fails" [] result

let test_string_p_success () =
  let result = run_parser (string_p "hello") "hello world" in
  check string_pair_list "string_p matches" [ (" world", "hello") ] result

let test_string_p_failure () =
  let result = run_parser (string_p "hello") "hi world" in
  check string_pair_list "string_p fails" [] result

let test_string_p_partial () =
  let result = run_parser (string_p "hello") "hel" in
  check string_pair_list "string_p partial fails" [] result

let test_pure () =
  let result = run_parser (pure 42) "anything" in
  check int_pair_list "pure returns value" [ ("anything", 42) ] result

let test_empty () =
  let result = run_parser empty "anything" in
  check int_pair_list "empty always fails" [] result

let test_eof_at_end () =
  let result = run_parser eof "" in
  check unit_pair_list "eof at end" [ ("", ()) ] result

let test_eof_not_at_end () =
  let result = run_parser eof "text" in
  check unit_pair_list "eof not at end" [] result

(** {1 Functor Tests} *)

let test_fmap () =
  let p = fmap (fun c -> Char.code c) (char_p 'a') in
  let result = run_parser p "abc" in
  check int_pair_list "fmap transforms result" [ ("bc", 97) ] result

let test_fmap_operator () =
  let p = String.uppercase_ascii <$> string_p "hello" in
  let result = run_parser p "hello world" in
  check string_pair_list "<$> operator works" [ (" world", "HELLO") ] result

let test_replace_left () =
  let p = 42 <$ char_p 'x' in
  let result = run_parser p "xyz" in
  check int_pair_list "<$ replaces result" [ ("yz", 42) ] result

let test_replace_right () =
  let p = char_p 'x' $> 42 in
  let result = run_parser p "xyz" in
  check int_pair_list "$> replaces result" [ ("yz", 42) ] result

(** {1 Applicative Tests} *)

let test_apply () =
  let pf = pure (fun x -> x + 1) in
  let px = pure 41 in
  let result = run_parser (pf <*> px) "" in
  check int_pair_list "apply works" [ ("", 42) ] result

let test_apply_sequence () =
  let p = (fun a b -> String.make 1 a ^ String.make 1 b) <$> char_p 'a' <*> char_p 'b' in
  let result = run_parser p "abc" in
  check string_pair_list "applicative sequence" [ ("c", "ab") ] result

let test_sequence_right () =
  let p = char_p 'a' *> char_p 'b' in
  let result = run_parser p "abc" in
  check char_pair_list "*> keeps right" [ ("c", 'b') ] result

let test_sequence_left () =
  let p = char_p 'a' <* char_p 'b' in
  let result = run_parser p "abc" in
  check char_pair_list "<* keeps left" [ ("c", 'a') ] result

let test_lift2 () =
  let p = lift2 ( + ) (pure 10) (pure 32) in
  let result = run_parser p "" in
  check int_pair_list "lift2 works" [ ("", 42) ] result

let test_lift3 () =
  let p = lift3 (fun a b c -> a + b + c) (pure 10) (pure 20) (pure 12) in
  let result = run_parser p "" in
  check int_pair_list "lift3 works" [ ("", 42) ] result

(** {1 Alternative Tests} *)

let test_alt_first () =
  let p = char_p 'a' <|> char_p 'b' in
  let result = run_parser p "abc" in
  check char_pair_list "alt chooses first" [ ("bc", 'a') ] result

let test_alt_second () =
  let p = char_p 'a' <|> char_p 'b' in
  let result = run_parser p "bcd" in
  check char_pair_list "alt chooses second" [ ("cd", 'b') ] result

let test_alt_both_fail () =
  let p = char_p 'a' <|> char_p 'b' in
  let result = run_parser p "xyz" in
  check char_pair_list "alt both fail" [] result

let test_many_some () =
  let p = many (char_p 'a') in
  let result = parse_string p "aaa" in
  check (option (list char)) "many parses multiple" (Some [ 'a'; 'a'; 'a' ]) result

let test_many_none () =
  let p = many (char_p 'a') in
  let result = parse_string p "" in
  check (option (list char)) "many parses zero" (Some []) result

let test_some_success () =
  let p = some (char_p 'a') in
  let result = parse_string p "aaa" in
  check (option (list char)) "some parses one or more" (Some [ 'a'; 'a'; 'a' ]) result

let test_some_failure () =
  let p = some (char_p 'a') in
  let result = parse_string p "bbb" in
  check (option (list char)) "some requires at least one" None result

let test_optional_some () =
  let p = optional (char_p 'a') <* eof in
  let result = parse_string p "a" in
  check (option (option char)) "optional with value" (Some (Some 'a')) result

let test_optional_none () =
  let p = optional (char_p 'a') in
  let result = parse_string p "" in
  check (option (option char)) "optional without value" (Some None) result

(** {1 String Parser Tests} *)

let test_skip_string () =
  let p = skip_string "hello" *> string_p "world" in
  let result = run_parser p "helloworld" in
  check string_pair_list "skip_string works" [ ("", "world") ] result

let test_skip_while () =
  let p = skip_while (fun c -> c = ' ') *> string_p "hello" in
  let result = run_parser p "   hello" in
  check string_pair_list "skip_while works" [ ("", "hello") ] result

let test_take_while () =
  let p = take_while (fun c -> c >= 'a' && c <= 'z') in
  let result = run_parser p "hello123" in
  check string_pair_list "take_while works" [ ("123", "hello") ] result

let test_take_while1_success () =
  let p = take_while1 (fun c -> c >= 'a' && c <= 'z') in
  let result = run_parser p "hello123" in
  check string_pair_list "take_while1 succeeds" [ ("123", "hello") ] result

let test_take_while1_failure () =
  let p = take_while1 (fun c -> c >= 'a' && c <= 'z') in
  let result = run_parser p "123hello" in
  check string_pair_list "take_while1 fails on empty" [] result

let test_skip_spaces () =
  let p = skip_spaces *> string_p "hello" in
  let result = run_parser p "  \t\nhello" in
  check string_pair_list "skip_spaces works" [ ("", "hello") ] result

(** {1 Character Class Tests} *)

let test_digit () =
  let result = run_parser digit "123" in
  check char_pair_list "digit parses" [ ("23", '1') ] result

let test_letter () =
  let result = run_parser letter "abc" in
  check char_pair_list "letter parses" [ ("bc", 'a') ] result

let test_alphanum () =
  let p = some alphanum in
  let result = parse_string p "abc123" in
  check (option (list char)) "alphanum parses" (Some [ 'a'; 'b'; 'c'; '1'; '2'; '3' ]) result

(** {1 Numeric Parser Tests} *)

let test_natural () =
  let result = run_parser natural "12345abc" in
  check int_pair_list "natural parses" [ ("abc", 12345) ] result

let test_natural_single () =
  let result = run_parser natural "5" in
  check int_pair_list "natural single digit" [ ("", 5) ] result

let test_integer_positive () =
  let result = run_parser integer "42abc" in
  check int_pair_list "integer positive" [ ("abc", 42) ] result

let test_integer_negative () =
  let result = run_parser integer "-42abc" in
  check int_pair_list "integer negative" [ ("abc", -42) ] result

(** {1 Combinator Tests} *)

let test_between () =
  let p = between (char_p '(') (char_p ')') (string_p "hello") in
  let result = run_parser p "(hello)" in
  check string_pair_list "between works" [ ("", "hello") ] result

let test_sep_by () =
  let p = sep_by (char_p ',') digit in
  let results = run_parser p "1,2,3" in
  (* Check that one of the results is the complete parse *)
  let has_complete =
    List.exists (fun (rest, chars) -> rest = "" && chars = [ '1'; '2'; '3' ]) results
  in
  check bool "sep_by works" true has_complete

let test_sep_by_empty () =
  let p = sep_by (char_p ',') digit in
  let result = parse_string p "" in
  check (option (list char)) "sep_by empty" (Some []) result

let test_sep_by1 () =
  let p = sep_by1 (char_p ',') digit in
  let result = parse_string p "1,2,3" in
  check (option (list char)) "sep_by1 works" (Some [ '1'; '2'; '3' ]) result

let test_choice () =
  let p = choice [ char_p 'a'; char_p 'b'; char_p 'c' ] in
  let result = run_parser p "bcd" in
  check char_pair_list "choice works" [ ("cd", 'b') ] result

let test_count () =
  let p = count 3 (char_p 'a') in
  let result = parse_string p "aaa" in
  check (option (list char)) "count works" (Some [ 'a'; 'a'; 'a' ]) result

let test_one_of () =
  let p = one_of "aeiou" in
  let result = run_parser p "echo" in
  check char_pair_list "one_of works" [ ("cho", 'e') ] result

let test_none_of () =
  let p = none_of "aeiou" in
  let result = run_parser p "hello" in
  check char_pair_list "none_of works" [ ("ello", 'h') ] result

(** {1 Expression Parser Tests} *)

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

(** {1 Test Suites} *)

let basic_tests =
  [
    test_case "satisfy success" `Quick test_satisfy_success;
    test_case "satisfy failure" `Quick test_satisfy_failure;
    test_case "satisfy empty" `Quick test_satisfy_empty;
    test_case "char_p success" `Quick test_char_p_success;
    test_case "char_p failure" `Quick test_char_p_failure;
    test_case "string_p success" `Quick test_string_p_success;
    test_case "string_p failure" `Quick test_string_p_failure;
    test_case "string_p partial" `Quick test_string_p_partial;
    test_case "pure" `Quick test_pure;
    test_case "empty" `Quick test_empty;
    test_case "eof at end" `Quick test_eof_at_end;
    test_case "eof not at end" `Quick test_eof_not_at_end;
  ]

let functor_tests =
  [
    test_case "fmap" `Quick test_fmap;
    test_case "<$> operator" `Quick test_fmap_operator;
    test_case "<$ replace left" `Quick test_replace_left;
    test_case "$> replace right" `Quick test_replace_right;
  ]

let applicative_tests =
  [
    test_case "apply" `Quick test_apply;
    test_case "apply sequence" `Quick test_apply_sequence;
    test_case "*> sequence right" `Quick test_sequence_right;
    test_case "<* sequence left" `Quick test_sequence_left;
    test_case "lift2" `Quick test_lift2;
    test_case "lift3" `Quick test_lift3;
  ]

let alternative_tests =
  [
    test_case "alt first" `Quick test_alt_first;
    test_case "alt second" `Quick test_alt_second;
    test_case "alt both fail" `Quick test_alt_both_fail;
    test_case "many some" `Quick test_many_some;
    test_case "many none" `Quick test_many_none;
    test_case "some success" `Quick test_some_success;
    test_case "some failure" `Quick test_some_failure;
    test_case "optional some" `Quick test_optional_some;
    test_case "optional none" `Quick test_optional_none;
  ]

let string_parser_tests =
  [
    test_case "skip_string" `Quick test_skip_string;
    test_case "skip_while" `Quick test_skip_while;
    test_case "take_while" `Quick test_take_while;
    test_case "take_while1 success" `Quick test_take_while1_success;
    test_case "take_while1 failure" `Quick test_take_while1_failure;
    test_case "skip_spaces" `Quick test_skip_spaces;
  ]

let char_class_tests =
  [
    test_case "digit" `Quick test_digit;
    test_case "letter" `Quick test_letter;
    test_case "alphanum" `Quick test_alphanum;
  ]

let numeric_tests =
  [
    test_case "natural" `Quick test_natural;
    test_case "natural single" `Quick test_natural_single;
    test_case "integer positive" `Quick test_integer_positive;
    test_case "integer negative" `Quick test_integer_negative;
  ]

let combinator_tests =
  [
    test_case "between" `Quick test_between;
    test_case "sep_by" `Quick test_sep_by;
    test_case "sep_by empty" `Quick test_sep_by_empty;
    test_case "sep_by1" `Quick test_sep_by1;
    test_case "choice" `Quick test_choice;
    test_case "count" `Quick test_count;
    test_case "one_of" `Quick test_one_of;
    test_case "none_of" `Quick test_none_of;
  ]

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

let () =
  run "Parser Combinator"
    [
      ("Basic Parsers", basic_tests);
      ("Functor", functor_tests);
      ("Applicative", applicative_tests);
      ("Alternative", alternative_tests);
      ("String Parsers", string_parser_tests);
      ("Character Classes", char_class_tests);
      ("Numeric Parsers", numeric_tests);
      ("Combinators", combinator_tests);
      ("Expression Parser", expr_parser_tests);
    ]
