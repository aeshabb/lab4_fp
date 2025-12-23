(** Demo application for the parser combinator library *)

open Parser_combinator.Expr_parser

let () =
  print_endline "=== Applicative Parser Combinator Demo ===\n";
  print_endline "Enter arithmetic expressions to parse and evaluate.";
  print_endline "Grammar:";
  print_endline "  expr      ::= constExpr | binOpExpr | negExpr";
  print_endline "  const     ::= int";
  print_endline "  binOpExpr ::= '(' expr ' ' binOp ' ' expr ')'";
  print_endline "  binOp     ::= '+' | '*'";
  print_endline "  negExpr   ::= '-' expr\n";
  print_endline "Examples: 123, -(10 + 42), (1 + ((2 + 3) * (4 + 5)))\n";
  print_endline "Type 'quit' to exit.\n";

  let rec loop () =
    print_string "> ";
    flush stdout;
    let line = read_line () in
    if line = "quit" then print_endline "Goodbye!"
    else begin
      (match parse line with
      | Some expr ->
          Printf.printf "Parsed: %s\n" (string_of_expr expr);
          Printf.printf "Result: %d\n" (eval expr)
      | None -> print_endline "Parse error: invalid expression");
      print_newline ();
      loop ()
    end
  in
  loop ()
