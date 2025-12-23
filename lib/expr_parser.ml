(** Парсер арифметических выражений

    Этот модуль реализует парсер простых арифметических выражений по грамматике из статьи на
    Habr:

    {v
    expr      ::= constExpr | binOpExpr | negExpr
    const     ::= int
    int       ::= digit{digit}
    digit     ::= '0' | ... | '9'
    binOpExpr ::= '(' expr ' ' binOp ' ' expr ')'
    binOp     ::= '+' | '*'
    negExpr   ::= '-' expr
    v}

    Примеры корректных выражений:
    - "123"
    - "-(10 + 42)"
    - "(1 + ((2 + 3) * (4 + 5)))" *)

open Parser

(** Бинарные операторы *)
type operator = Add | Mul

(** AST выражений *)
type expr = ConstExpr of int | BinaryExpr of expr * operator * expr | NegateExpr of expr

(** Преобразовать оператор в строку *)
let string_of_operator = function Add -> "+" | Mul -> "*"

(** Преобразовать выражение в строку *)
let rec string_of_expr = function
  | ConstExpr n -> string_of_int n
  | BinaryExpr (e1, op, e2) ->
      Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_operator op) (string_of_expr e2)
  | NegateExpr e -> Printf.sprintf "-%s" (string_of_expr e)

(** Вычислить значение выражения до целого числа *)
let rec eval = function
  | ConstExpr n -> n
  | BinaryExpr (e1, Add, e2) -> eval e1 + eval e2
  | BinaryExpr (e1, Mul, e2) -> eval e1 * eval e2
  | NegateExpr e -> -eval e

(** Парсер константных целочисленных выражений *)
let const_parser : expr t = (fun n -> ConstExpr n) <$> natural

(** Парсер бинарных операторов *)
let bin_op_parser : operator t =
  let plus_parser = Add <$ char_p '+' in
  let mult_parser = Mul <$ char_p '*' in
  plus_parser <|> mult_parser

(** Рекурсивный парсер выражений — использует thunk-и для ленивого вычисления *)
let rec expr_parser_thunk () : expr t = const_parser <|> bin_parser_thunk () <|> neg_parser_thunk ()

(** Парсер бинарных выражений: '(' expr ' ' binOp ' ' expr ')' *)
and bin_parser_thunk () : expr t =
  let open_paren = char_p '(' in
  let close_paren = char_p ')' in
  let space = char_p ' ' in
  let make_binary e1 op e2 = BinaryExpr (e1, op, e2) in
  let expr = lazy_p expr_parser_thunk in
  open_paren *> lift3 make_binary expr (space *> bin_op_parser <* space) expr <* close_paren

(** Парсер отрицания: '-' expr *)
and neg_parser_thunk () : expr t =
  (fun e -> NegateExpr e) <$> char_p '-' *> lazy_p expr_parser_thunk

(** Главный парсер выражений *)
let expression : expr t = lazy_p expr_parser_thunk

(** Разобрать и вычислить выражение *)
let parse_and_eval s =
  match parse_string expression s with Some expr -> Some (eval expr) | None -> None

(** Разобрать выражение в AST *)
let parse s = parse_string expression s
