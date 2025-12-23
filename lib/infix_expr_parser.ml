(** Парсер арифметических выражений с приоритетами (без обязательных скобок)

    Грамматика:
    {v
    expr   ::= term (('+' | '-') term)*
    term   ::= factor (('*' | '/') factor)*
    factor ::= number | '-' factor | '(' expr ')'
    number ::= digit+
    v}

    Примеры:
    - "1 + 2 * 3"  → 7
    - "2 * 3 + 4"  → 10
    - "-(5 + 3)"   → -8
    - "10 / 2 - 1" → 4
*)

open Parser
open Syntax

(** Бинарные операторы *)
type binop = Add | Sub | Mul | Div

(** Выражения *)
type expr =
  | Const of int
  | BinOp of expr * binop * expr
  | Negate of expr

(** Преобразовать оператор в строку *)
let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

(** Преобразовать выражение в строку *)
let rec string_of_expr = function
  | Const n -> string_of_int n
  | BinOp (e1, op, e2) ->
    Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_binop op) (string_of_expr e2)
  | Negate e -> Printf.sprintf "-%s" (string_of_expr e)

(** Вычислить выражение *)
let rec eval = function
  | Const n -> n
  | BinOp (e1, Add, e2) -> eval e1 + eval e2
  | BinOp (e1, Sub, e2) -> eval e1 - eval e2
  | BinOp (e1, Mul, e2) -> eval e1 * eval e2
  | BinOp (e1, Div, e2) -> eval e1 / eval e2
  | Negate e -> -eval e

(** Вспомогательная функция для построения левоассоциативного бинарного дерева *)
let make_binop_left init ops_and_operands =
  List.fold_left (fun acc (op, operand) -> BinOp (acc, op, operand)) init ops_and_operands

(** Парсер числа *)
let number : int t =
  skip_spaces *> integer <* skip_spaces

(** Форвард-объявления для взаимной рекурсии *)
let expr_ref : expr t option ref = ref None
let factor_ref : expr t option ref = ref None
let expr () = match !expr_ref with Some p -> p | None -> failwith "expr not initialized"
let get_factor () = match !factor_ref with Some p -> p | None -> failwith "factor not initialized"

(** Парсер фактора: число | '-' фактор | '(' выражение ')' *)
let factor : expr t =
  lazy_p (fun () ->
    let paren_parser =
      let+ e = between (char_p '(' *> skip_spaces) (skip_spaces *> char_p ')') (expr ()) in
      e
    in
    let neg_parser =
      let+ e = char_p '-' *> skip_spaces *> get_factor () in
      Negate e
    in
    let const_parser =
      let+ n = number in
      Const n
    in
    paren_parser <|> neg_parser <|> const_parser)

(** Парсер терма: фактор (('*' | '/') фактор)* *)
let term : expr t =
  let mul_op = skip_spaces *> ((Mul <$ char_p '*') <|> (Div <$ char_p '/')) <* skip_spaces in
  let+ init = factor
  and+ rest = many (let+ o = mul_op and+ operand = factor in (o, operand)) in
  make_binop_left init rest

(** Парсер выражения: терм (('+' | '-') терм)* *)
let expression : expr t =
  let add_op = skip_spaces *> ((Add <$ char_p '+') <|> (Sub <$ char_p '-')) <* skip_spaces in
  let+ init = skip_spaces *> term
  and+ rest = many (let+ o = add_op and+ operand = term in (o, operand)) in
  make_binop_left init rest

(* Инициализация форвард-ссылки *)
let () = factor_ref := Some factor
let () = expr_ref := Some expression

(** Разобрать строку в выражение *)
let parse s = parse_string expression s

(** Разобрать и вычислить *)
let parse_and_eval s =
  match parse s with
  | Some e -> Some (eval e)
  | None -> None
