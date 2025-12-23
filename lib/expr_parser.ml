(** Arithmetic Expression Parser

    This module implements a parser for simple arithmetic expressions following the grammar from the
    Habr article:

    {v
    expr      ::= constExpr | binOpExpr | negExpr
    const     ::= int
    int       ::= digit{digit}
    digit     ::= '0' | ... | '9'
    binOpExpr ::= '(' expr ' ' binOp ' ' expr ')'
    binOp     ::= '+' | '*'
    negExpr   ::= '-' expr
    v}

    Examples of valid expressions:
    - "123"
    - "-(10 + 42)"
    - "(1 + ((2 + 3) * (4 + 5)))" *)

open Parser

(** Binary operators *)
type operator = Add | Mul

(** Expression AST *)
type expr = ConstExpr of int | BinaryExpr of expr * operator * expr | NegateExpr of expr

(** Convert operator to string *)
let string_of_operator = function Add -> "+" | Mul -> "*"

(** Convert expression to string *)
let rec string_of_expr = function
  | ConstExpr n -> string_of_int n
  | BinaryExpr (e1, op, e2) ->
      Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_operator op) (string_of_expr e2)
  | NegateExpr e -> Printf.sprintf "-%s" (string_of_expr e)

(** Evaluate an expression to an integer *)
let rec eval = function
  | ConstExpr n -> n
  | BinaryExpr (e1, Add, e2) -> eval e1 + eval e2
  | BinaryExpr (e1, Mul, e2) -> eval e1 * eval e2
  | NegateExpr e -> -eval e

(** Parser for integer constant expressions *)
let const_parser : expr t = (fun n -> ConstExpr n) <$> natural

(** Parser for binary operators *)
let bin_op_parser : operator t =
  let plus_parser = Add <$ char_p '+' in
  let mult_parser = Mul <$ char_p '*' in
  plus_parser <|> mult_parser

(** Parser for expressions (recursive) - using thunks for lazy evaluation *)
let rec expr_parser_thunk () : expr t = const_parser <|> bin_parser_thunk () <|> neg_parser_thunk ()

(** Parser for binary operation expressions: '(' expr ' ' binOp ' ' expr ')' *)
and bin_parser_thunk () : expr t =
  let open_paren = char_p '(' in
  let close_paren = char_p ')' in
  let space = char_p ' ' in
  let make_binary e1 op e2 = BinaryExpr (e1, op, e2) in
  let expr = lazy_p expr_parser_thunk in
  open_paren *> lift3 make_binary expr (space *> bin_op_parser <* space) expr <* close_paren

(** Parser for negation expressions: '-' expr *)
and neg_parser_thunk () : expr t =
  (fun e -> NegateExpr e) <$> char_p '-' *> lazy_p expr_parser_thunk

(** Main expression parser *)
let expression : expr t = lazy_p expr_parser_thunk

(** Parse and evaluate an expression string *)
let parse_and_eval s =
  match parse_string expression s with Some expr -> Some (eval expr) | None -> None

(** Parse an expression string to AST *)
let parse s = parse_string expression s
