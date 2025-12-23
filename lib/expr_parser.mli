(** Arithmetic Expression Parser

    This module implements a parser for simple arithmetic expressions following the grammar from the
    Habr article. *)

(** Binary operators *)
type operator = Add | Mul

(** Expression AST *)
type expr = ConstExpr of int | BinaryExpr of expr * operator * expr | NegateExpr of expr

val string_of_operator : operator -> string
(** Convert operator to string *)

val string_of_expr : expr -> string
(** Convert expression to string *)

val eval : expr -> int
(** Evaluate an expression to an integer *)

val const_parser : expr Parser.t
(** Parser for constant expressions *)

val bin_op_parser : operator Parser.t
(** Parser for binary operators *)

val expression : expr Parser.t
(** Main expression parser *)

val parse_and_eval : string -> int option
(** Parse and evaluate an expression string *)

val parse : string -> expr option
(** Parse an expression string to AST *)
