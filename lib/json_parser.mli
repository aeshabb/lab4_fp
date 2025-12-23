(** Простейший JSON парсер *)

(** Типы JSON *)
type json =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of json list
  | Object of (string * json) list

val to_string : json -> string
(** Преобразовать JSON в строку *)

val json : json Parser.t
(** Главный парсер JSON *)

val parse : string -> json option
(** Разобрать строку *)

val parse_and_print : string -> string option
(** Разобрать и преобразовать в строку *)
