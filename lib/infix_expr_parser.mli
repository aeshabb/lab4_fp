(** Парсер арифметических выражений с приоритетами (без обязательных скобок) *)

(** Бинарные операторы *)
type binop = Add | Sub | Mul | Div

(** Выражения *)
type expr = Const of int | BinOp of expr * binop * expr | Negate of expr

val string_of_binop : binop -> string
(** Преобразовать оператор в строку *)

val string_of_expr : expr -> string
(** Преобразовать выражение в строку *)

val eval : expr -> int
(** Вычислить выражение *)

val expression : expr Parser.t
(** Главный парсер выражений *)

val parse : string -> expr option
(** Разобрать строку в выражение *)

val parse_and_eval : string -> int option
(** Разобрать и вычислить *)
