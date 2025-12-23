(** Парсер арифметических выражений

    Этот модуль реализует парсер простых арифметических выражений по грамматике из статьи на Habr.
*)

(** Бинарные операторы *)
type operator = Add | Mul

(** AST выражений *)
type expr = ConstExpr of int | BinaryExpr of expr * operator * expr | NegateExpr of expr

val string_of_operator : operator -> string
(** Преобразовать оператор в строку *)

val string_of_expr : expr -> string
(** Преобразовать выражение в строку *)

val eval : expr -> int
(** Вычислить значение выражения до целого числа *)

val const_parser : expr Parser.t
(** Парсер константных выражений *)

val bin_op_parser : operator Parser.t
(** Парсер бинарных операторов *)

val expression : expr Parser.t
(** Главный парсер выражений *)

val parse_and_eval : string -> int option
(** Разобрать и вычислить выражение *)

val parse : string -> expr option
(** Разобрать выражение в AST *)
