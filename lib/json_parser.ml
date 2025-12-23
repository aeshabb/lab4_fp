(** Простейший JSON парсер

    Поддерживает: null, bool, number, string, array, object *)

open Parser
open Syntax

(** Типы JSON *)
type json =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | Array of json list
  | Object of (string * json) list

(** Преобразовать JSON в строку *)
let rec to_string = function
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Number n ->
      (* Выводить целые числа без .0 *)
      if Float.is_integer n then string_of_int (int_of_float n) else string_of_float n
  | String s -> Printf.sprintf "\"%s\"" s
  | Array items -> "[" ^ String.concat ", " (List.map to_string items) ^ "]"
  | Object pairs ->
      let pair_to_str (k, v) = Printf.sprintf "\"%s\": %s" k (to_string v) in
      "{" ^ String.concat ", " (List.map pair_to_str pairs) ^ "}"

(** Парсер null *)
let null_parser : json t =
  let+ _ = string_p "null" in
  Null

(** Парсер bool *)
let bool_parser : json t =
  let true_p = Bool true <$ string_p "true" in
  let false_p = Bool false <$ string_p "false" in
  true_p <|> false_p

(** Парсер числа (упрощённый) *)
let number_parser : json t =
  let+ n = integer in
  Number (float_of_int n)

(** Парсер строки (упрощённый, без эскейпов) *)
let string_parser : json t =
  let content = take_while (fun c -> c <> '"' && c <> '\\') in
  let+ s = char_p '"' *> content <* char_p '"' in
  String s

(** Форвард-объявление для рекурсии *)
let json_value_ref : json t option ref = ref None

let json_value () =
  match !json_value_ref with Some p -> p | None -> failwith "json_value not initialized"

(** Парсер массива *)
let array_parser : json t =
  lazy_p (fun () ->
      let element = skip_spaces *> json_value () <* skip_spaces in
      let+ items =
        between
          (char_p '[' *> skip_spaces)
          (skip_spaces *> char_p ']')
          (sep_by (char_p ',' *> skip_spaces) element)
      in
      Array items)

(** Парсер объекта *)
let object_parser : json t =
  lazy_p (fun () ->
      let key_parser =
        let content = take_while (fun c -> c <> '"' && c <> '\\') in
        char_p '"' *> content <* char_p '"' <* skip_spaces
      in
      let pair_parser =
        let+ key = key_parser <* char_p ':' <* skip_spaces
        and+ value = json_value () <* skip_spaces in
        (key, value)
      in
      let+ pairs =
        between
          (char_p '{' *> skip_spaces)
          (skip_spaces *> char_p '}')
          (sep_by (char_p ',' *> skip_spaces) pair_parser)
      in
      Object pairs)

(** Главный парсер JSON значения *)
let json : json t =
  lazy_p (fun () ->
      skip_spaces
      *> (null_parser <|> bool_parser <|> number_parser <|> string_parser <|> array_parser
        <|> object_parser)
      <* skip_spaces)

(* Инициализация *)
let () = json_value_ref := Some json

(** Разобрать строку *)
let parse s = parse_string json s

(** Разобрать и преобразовать в строку *)
let parse_and_print s = match parse s with Some v -> Some (to_string v) | None -> None
