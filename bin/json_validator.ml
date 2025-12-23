(** CLI утилита для валидации JSON файлов *)

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <json-file>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in
  let content = In_channel.with_open_text filename In_channel.input_all in

  match Parser_combinator.Json_parser.parse content with
  | Some json ->
    print_endline "Valid JSON";
    print_endline (Parser_combinator.Json_parser.to_string json)
  | None ->
    Printf.eprintf "Invalid JSON\n";
    exit 1
