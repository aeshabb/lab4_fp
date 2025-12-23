(** Демонстрационное приложение для библиотеки парсер-комбинаторов *)

open Parser_combinator.Expr_parser

let () =
  print_endline "=== Демо аппликативных парсер-комбинаторов ===\n";
  print_endline "Введите арифметические выражения для разбора и вычисления.";
  print_endline "Грамматика:";
  print_endline "  expr      ::= constExpr | binOpExpr | negExpr";
  print_endline "  const     ::= int";
  print_endline "  binOpExpr ::= '(' expr ' ' binOp ' ' expr ')'";
  print_endline "  binOp     ::= '+' | '*'";
  print_endline "  negExpr   ::= '-' expr\n";
  print_endline "Примеры: 123, -(10 + 42), (1 + ((2 + 3) * (4 + 5)))\n";
  print_endline "Введите 'quit' для выхода.\n";

  let rec loop () =
    print_string "> ";
    flush stdout;
    let line = read_line () in
    if line = "quit" then print_endline "Пока!"
    else begin
      (match parse line with
       | Some expr ->
         Printf.printf "Разобрано: %s\n" (string_of_expr expr);
         Printf.printf "Результат: %d\n" (eval expr)
       | None -> print_endline "Ошибка разбора: некорректное выражение");
      print_newline ();
      loop ()
    end
  in
  loop ()
