(** Парсер значений типа ['a]. Внутри это функция из строки в список пар (оставшаяся_строка,
    значение). *)
type 'a t = Parser of (string -> (string * 'a) list)

(** Распаковать функцию парсера *)
let run_parser (Parser p) = p

(** Попробовать разобрать строку целиком. Возвращает [Some value], если разбор успешен и не осталось
    непрочитанного ввода, и [None] в противном случае. *)
let parse_string (Parser p) s = match p s with [ ("", value) ] -> Some value | _ -> None

(** Разобрать строку и вернуть все возможные результаты *)
let parse_all (Parser p) s = p s

(** {1 Базовые парсеры} *)

(** Парсер, принимающий один символ, удовлетворяющий предикату *)
let satisfy pred =
  Parser
    (fun s ->
      match s with
      | "" -> []
      | s when String.length s > 0 ->
          let c = s.[0] in
          if pred c then [ (String.sub s 1 (String.length s - 1), c) ] else []
      | _ -> [])

(** Синоним для [satisfy] — совпадает с названием из статьи на Haskell *)
let pred_p = satisfy

(** Парсер для конкретного символа *)
let char_p c = satisfy (fun x -> x = c)

(** Парсер, который всегда завершается неуспехом *)
let empty = Parser (fun _ -> [])

(** Парсер, который всегда успешно завершается, не потребляя вход, и возвращает заданное значение *)
let pure x = Parser (fun s -> [ (s, x) ])

(** Ленивый обёрточный парсер для организации рекурсии без переполнения стека *)
let lazy_p (thunk : unit -> 'a t) : 'a t = Parser (fun s -> run_parser (thunk ()) s)

(** Парсер, который успешно завершается только в конце ввода *)
let eof = Parser (fun s -> match s with "" -> [ ("", ()) ] | _ -> [])

(** {1 Операции функтора} *)

(** Применить функцию к результату парсера *)
let fmap f (Parser p) = Parser (fun s -> List.map (fun (rest, value) -> (rest, f value)) (p s))

(** Инфиксный оператор для [fmap] *)
let ( <$> ) f p = fmap f p

(** Заменить результат константным значением *)
let ( <$ ) x p = fmap (fun _ -> x) p

(** Заменить результат константным значением (аргументы переставлены местами) *)
let ( $> ) p x = fmap (fun _ -> x) p

(** {1 Аппликативные операции} *)

(** Применить парсер с функцией к парсеру со значением *)
let apply (Parser pf) (Parser px) =
  Parser
    (fun s -> List.concat_map (fun (sf, f) -> List.map (fun (sx, x) -> (sx, f x)) (px sf)) (pf s))

(** Инфиксный оператор для [apply] *)
let ( <*> ) = apply

(** Последовательно выполнить два парсера, сохраняя только результат второго *)
let ( *> ) p1 p2 = (fun _ x -> x) <$> p1 <*> p2

(** Последовательно выполнить два парсера, сохраняя только результат первого *)
let ( <* ) p1 p2 = (fun x _ -> x) <$> p1 <*> p2

(** Поднять бинарную функцию на уровень парсеров *)
let lift2 f p1 p2 = f <$> p1 <*> p2

(** Поднять тернарную функцию на уровень парсеров *)
let lift3 f p1 p2 p3 = f <$> p1 <*> p2 <*> p3

(** {1 Альтернативные операции} *)

(** Сначала попробовать первый парсер, при неуспехе — второй *)
let alt (Parser p1) (Parser p2) = Parser (fun s -> p1 s @ p2 s)

(** Инфиксный оператор для [alt] *)
let ( <|> ) = alt

(** Разобрать ноль или больше вхождений — жадный разбор, сначала возвращает самое длинное совпадение
*)
let many (Parser p) =
  Parser
    (fun s ->
      let rec go acc s =
        match p s with
        | [] -> [ (s, List.rev acc) ]
        | results -> List.concat_map (fun (rest, x) -> go (x :: acc) rest) results
      in
      go [] s)

(** Разобрать одно или больше вхождений *)
let some (Parser p) =
  Parser
    (fun s ->
      match p s with
      | [] -> []
      | results ->
          let rec go acc s =
            match p s with
            | [] -> [ (s, List.rev acc) ]
            | results' -> List.concat_map (fun (rest, x) -> go (x :: acc) rest) results'
          in
          List.concat_map (fun (rest, x) -> go [ x ] rest) results)

(** Необязательный парсер — возвращает [Some x] при успехе и [None] при неуспехе *)
let optional p = (fun x -> Some x) <$> p <|> pure None

(** {1 Строковые парсеры} *)

(** Парсер для конкретного строкового префикса *)
let string_p str =
  let len = String.length str in
  Parser
    (fun s ->
      if String.length s >= len && String.sub s 0 len = str then
        [ (String.sub s len (String.length s - len), str) ]
      else [])

(** Парсер для строкового префикса, возвращающий [()] *)
let skip_string str = () <$ string_p str

(** Пропускать символы, пока предикат истинный *)
let skip_while pred =
  Parser
    (fun s ->
      let rec find_end i =
        if i >= String.length s then i else if pred s.[i] then find_end (i + 1) else i
      in
      let end_idx = find_end 0 in
      [ (String.sub s end_idx (String.length s - end_idx), ()) ])

(** Считать символы, пока предикат истинный *)
let take_while pred =
  Parser
    (fun s ->
      let rec find_end i =
        if i >= String.length s then i else if pred s.[i] then find_end (i + 1) else i
      in
      let end_idx = find_end 0 in
      [ (String.sub s end_idx (String.length s - end_idx), String.sub s 0 end_idx) ])

(** Считать как минимум один символ, пока предикат истинный *)
let take_while1 pred =
  Parser
    (fun s ->
      let rec find_end i =
        if i >= String.length s then i else if pred s.[i] then find_end (i + 1) else i
      in
      let end_idx = find_end 0 in
      if end_idx = 0 then []
      else [ (String.sub s end_idx (String.length s - end_idx), String.sub s 0 end_idx) ])

(** Пропустить все пробельные символы *)
let skip_spaces = skip_while (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r')

(** {1 Парсеры классов символов} *)

(** Разобрать цифру *)
let digit = satisfy (fun c -> c >= '0' && c <= '9')

(** Разобрать букву *)
let letter = satisfy (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

(** Разобрать букву или цифру *)
let alphanum = letter <|> digit

(** {1 Числовые парсеры} *)

(** Разобрать натуральное число (неотрицательное целое) *)
let natural =
  let digits_to_int s =
    String.fold_left (fun acc c -> (acc * 10) + (Char.code c - Char.code '0')) 0 s
  in
  fmap digits_to_int (take_while1 (fun c -> c >= '0' && c <= '9'))

(** Разобрать целое число (возможно, отрицательное) *)
let integer =
  let negate n = -n in
  negate <$> char_p '-' *> natural <|> natural

(** {1 Комбинаторы} *)

(** Разобрать что‑то между двумя другими парсерами *)
let between open_p close_p p = open_p *> p <* close_p

(** Разобрать элементы, разделённые разделителем (как минимум один элемент) *)
let sep_by1 sep p = (fun x xs -> x :: xs) <$> p <*> many (sep *> p)

(** Разобрать элементы, разделённые разделителем (возможно ноль элементов) *)
let sep_by sep p = sep_by1 sep p <|> pure []

(** Связать левосочетательные бинарные операции в цепочку *)
let chainl1 p op =
  let rec rest acc =
    (let apply_op f x = f acc x in
     apply_op <$> op <*> p >>= rest)
    <|> pure acc
  and ( >>= ) (Parser p) f =
    Parser (fun s -> List.concat_map (fun (rest, value) -> run_parser (f value) rest) (p s))
  in
  p >>= rest

(** Связать правосочетательные бинарные операции в цепочку *)
let chainr1 p op =
  let rec parse () = (fun x f -> f x) <$> p <*> rest ()
  and rest () = (fun f y x -> f x y) <$> op <*> parse () <|> pure (fun x -> x) in
  parse ()

(** {1 Вспомогательные функции} *)

(** Оператор связывания (монада) для последовательного комбинирования парсеров *)
let ( >>= ) (Parser p) f =
  Parser (fun s -> List.concat_map (fun (rest, value) -> run_parser (f value) rest) (p s))

(** Последовательное выполнение (монада), результат первого парсера отбрасывается *)
let ( >> ) p1 p2 = p1 >>= fun _ -> p2

(** Альтернатива из списка парсеров *)
let choice parsers = List.fold_left ( <|> ) empty parsers

(** Разобрать ровно [n] вхождений *)
let rec count n p = if n <= 0 then pure [] else (fun x xs -> x :: xs) <$> p <*> count (n - 1) p

(** Разобрать один из указанных символов *)
let one_of chars = satisfy (fun c -> String.contains chars c)

(** Разобрать символ, не входящий в указанный набор *)
let none_of chars = satisfy (fun c -> not (String.contains chars c))

(** Взгляд вперёд — попробовать парсер, не потребляя ввод *)
let look_ahead (Parser p) =
  Parser (fun s -> match p s with [] -> [] | (_, value) :: _ -> [ (s, value) ])

(** Условие «не следует за» — успешно, если указанный парсер завершился неуспехом *)
let not_followed_by (Parser p) = Parser (fun s -> match p s with [] -> [ (s, ()) ] | _ -> [])
