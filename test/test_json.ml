(** Юнит-тесты для JSON парсера *)

open Alcotest
open Parser_combinator.Json_parser

let json_testable =
  testable
    (fun fmt j -> Format.fprintf fmt "%s" (to_string j))
    ( = )

let json_option = option json_testable

(** {1 Тесты JSON парсера} *)

let test_null () =
  let result = parse "null" in
  check json_option "parse null" (Some Null) result

let test_bool_true () =
  let result = parse "true" in
  check json_option "parse true" (Some (Bool true)) result

let test_bool_false () =
  let result = parse "false" in
  check json_option "parse false" (Some (Bool false)) result

let test_number () =
  let result = parse "42" in
  check json_option "parse number" (Some (Number 42.0)) result

let test_string () =
  let result = parse "\"hello\"" in
  check json_option "parse string" (Some (String "hello")) result

let test_empty_array () =
  let result = parse "[]" in
  check json_option "parse empty array" (Some (Array [])) result

let test_array_with_numbers () =
  let result = parse "[1, 2, 3]" in
  check json_option "parse array with numbers"
    (Some (Array [Number 1.0; Number 2.0; Number 3.0])) result

let test_empty_object () =
  let result = parse "{}" in
  check json_option "parse empty object" (Some (Object [])) result

let test_simple_object () =
  let result = parse "{\"name\": \"test\", \"value\": 42}" in
  check json_option "parse simple object"
    (Some (Object [("name", String "test"); ("value", Number 42.0)])) result

let test_nested_object () =
  let result = parse "{\"user\": {\"name\": \"Alice\", \"age\": 30}}" in
  let expected = Some (Object [
    ("user", Object [("name", String "Alice"); ("age", Number 30.0)])
  ]) in
  check json_option "parse nested object" expected result

let test_array_of_objects () =
  let result = parse "[{\"x\": 1}, {\"y\": 2}]" in
  let expected = Some (Array [
    Object [("x", Number 1.0)];
    Object [("y", Number 2.0)]
  ]) in
  check json_option "parse array of objects" expected result

let test_mixed_array () =
  let result = parse "[null, true, 42, \"text\"]" in
  let expected = Some (Array [Null; Bool true; Number 42.0; String "text"]) in
  check json_option "parse mixed array" expected result

let test_invalid_json () =
  let result = parse "{invalid}" in
  check json_option "reject invalid JSON" None result

(** {1 Наборы тестов} *)

let json_tests =
  [
    test_case "null" `Quick test_null;
    test_case "bool true" `Quick test_bool_true;
    test_case "bool false" `Quick test_bool_false;
    test_case "number" `Quick test_number;
    test_case "string" `Quick test_string;
    test_case "empty array" `Quick test_empty_array;
    test_case "array with numbers" `Quick test_array_with_numbers;
    test_case "empty object" `Quick test_empty_object;
    test_case "simple object" `Quick test_simple_object;
    test_case "nested object" `Quick test_nested_object;
    test_case "array of objects" `Quick test_array_of_objects;
    test_case "mixed array" `Quick test_mixed_array;
    test_case "invalid JSON" `Quick test_invalid_json;
  ]

let () =
  run "JSON Parser" [("JSON Parser", json_tests)]
