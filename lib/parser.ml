(** Applicative Parser Combinator Library

    This module provides a functional parser combinator library inspired by Haskell's applicative
    parsers. It follows the approach described in "Applicative Parsers on Haskell" article.

    The parser is a function that takes a string and returns a list of possible parse results, where
    each result is a pair of (remaining string, parsed value). *)

(** {1 Core Types} *)

(** A parser for values of type ['a]. Internally, it's a function from string to a list of
    (remaining_string, value) pairs. *)
type 'a t = Parser of (string -> (string * 'a) list)

(** Unwrap the parser function *)
let run_parser (Parser p) = p

(** Try to parse a complete string. Returns [Some value] if parsing succeeds with no remaining
    input, [None] otherwise. *)
let parse_string (Parser p) s = match p s with [ ("", value) ] -> Some value | _ -> None

(** Parse a string and return all possible results *)
let parse_all (Parser p) s = p s

(** {1 Basic Parsers} *)

(** Parser that accepts a single character satisfying a predicate *)
let satisfy pred =
  Parser
    (fun s ->
      match s with
      | "" -> []
      | s when String.length s > 0 ->
          let c = s.[0] in
          if pred c then [ (String.sub s 1 (String.length s - 1), c) ] else []
      | _ -> [])

(** Alias for [satisfy] - matches Haskell naming from the article *)
let pred_p = satisfy

(** Parser for a specific character *)
let char_p c = satisfy (fun x -> x = c)

(** Parser that always fails *)
let empty = Parser (fun _ -> [])

(** Parser that succeeds without consuming input, returning the given value *)
let pure x = Parser (fun s -> [ (s, x) ])

(** Lazy parser wrapper to enable recursion without stack overflow *)
let lazy_p (thunk : unit -> 'a t) : 'a t = Parser (fun s -> run_parser (thunk ()) s)

(** Parser that returns the given value if at the end of input *)
let eof = Parser (fun s -> match s with "" -> [ ("", ()) ] | _ -> [])

(** {1 Functor Operations} *)

(** Map a function over the result of a parser *)
let fmap f (Parser p) = Parser (fun s -> List.map (fun (rest, value) -> (rest, f value)) (p s))

(** Infix operator for [fmap] *)
let ( <$> ) f p = fmap f p

(** Replace the result with a constant value *)
let ( <$ ) x p = fmap (fun _ -> x) p

(** Replace the result with a constant value (flipped) *)
let ( $> ) p x = fmap (fun _ -> x) p

(** {1 Applicative Operations} *)

(** Apply a parser containing a function to a parser containing a value *)
let apply (Parser pf) (Parser px) =
  Parser
    (fun s -> List.concat_map (fun (sf, f) -> List.map (fun (sx, x) -> (sx, f x)) (px sf)) (pf s))

(** Infix operator for [apply] *)
let ( <*> ) = apply

(** Sequence two parsers, keeping only the result of the second *)
let ( *> ) p1 p2 = (fun _ x -> x) <$> p1 <*> p2

(** Sequence two parsers, keeping only the result of the first *)
let ( <* ) p1 p2 = (fun x _ -> x) <$> p1 <*> p2

(** Lift a binary function to work on parsers *)
let lift2 f p1 p2 = f <$> p1 <*> p2

(** Lift a ternary function to work on parsers *)
let lift3 f p1 p2 p3 = f <$> p1 <*> p2 <*> p3

(** {1 Alternative Operations} *)

(** Try first parser, if it fails try second parser *)
let alt (Parser p1) (Parser p2) = Parser (fun s -> p1 s @ p2 s)

(** Infix operator for [alt] *)
let ( <|> ) = alt

(** Parse zero or more occurrences - greedy, returns longest match first *)
let many (Parser p) =
  Parser
    (fun s ->
      let rec go acc s =
        match p s with
        | [] -> [ (s, List.rev acc) ]
        | results -> List.concat_map (fun (rest, x) -> go (x :: acc) rest) results
      in
      go [] s)

(** Parse one or more occurrences *)
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

(** Optional parser - returns [Some x] if succeeds, [None] otherwise *)
let optional p = (fun x -> Some x) <$> p <|> pure None

(** {1 String Parsers} *)

(** Parser for a specific string prefix *)
let string_p str =
  let len = String.length str in
  Parser
    (fun s ->
      if String.length s >= len && String.sub s 0 len = str then
        [ (String.sub s len (String.length s - len), str) ]
      else [])

(** Parser for a string prefix, returns unit *)
let skip_string str = () <$ string_p str

(** Skip characters while predicate holds *)
let skip_while pred =
  Parser
    (fun s ->
      let rec find_end i =
        if i >= String.length s then i else if pred s.[i] then find_end (i + 1) else i
      in
      let end_idx = find_end 0 in
      [ (String.sub s end_idx (String.length s - end_idx), ()) ])

(** Take characters while predicate holds *)
let take_while pred =
  Parser
    (fun s ->
      let rec find_end i =
        if i >= String.length s then i else if pred s.[i] then find_end (i + 1) else i
      in
      let end_idx = find_end 0 in
      [ (String.sub s end_idx (String.length s - end_idx), String.sub s 0 end_idx) ])

(** Take at least one character while predicate holds *)
let take_while1 pred =
  Parser
    (fun s ->
      let rec find_end i =
        if i >= String.length s then i else if pred s.[i] then find_end (i + 1) else i
      in
      let end_idx = find_end 0 in
      if end_idx = 0 then []
      else [ (String.sub s end_idx (String.length s - end_idx), String.sub s 0 end_idx) ])

(** Skip whitespace characters *)
let skip_spaces = skip_while (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r')

(** {1 Character Class Parsers} *)

(** Parse a digit character *)
let digit = satisfy (fun c -> c >= '0' && c <= '9')

(** Parse a letter character *)
let letter = satisfy (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

(** Parse an alphanumeric character *)
let alphanum = letter <|> digit

(** {1 Numeric Parsers} *)

(** Parse a natural number (non-negative integer) *)
let natural =
  let digits_to_int s =
    String.fold_left (fun acc c -> (acc * 10) + (Char.code c - Char.code '0')) 0 s
  in
  fmap digits_to_int (take_while1 (fun c -> c >= '0' && c <= '9'))

(** Parse an integer (optionally negative) *)
let integer =
  let negate n = -n in
  negate <$> char_p '-' *> natural <|> natural

(** {1 Combinators} *)

(** Parse something between two other parsers *)
let between open_p close_p p = open_p *> p <* close_p

(** Parse items separated by a separator (at least one item) *)
let sep_by1 sep p = (fun x xs -> x :: xs) <$> p <*> many (sep *> p)

(** Parse items separated by a separator *)
let sep_by sep p = sep_by1 sep p <|> pure []

(** Chain left-associative binary operations *)
let chainl1 p op =
  let rec rest acc =
    (let apply_op f x = f acc x in
     apply_op <$> op <*> p >>= rest)
    <|> pure acc
  and ( >>= ) (Parser p) f =
    Parser (fun s -> List.concat_map (fun (rest, value) -> run_parser (f value) rest) (p s))
  in
  p >>= rest

(** Chain right-associative binary operations *)
let chainr1 p op =
  let rec parse () = (fun x f -> f x) <$> p <*> rest ()
  and rest () = (fun f y x -> f x y) <$> op <*> parse () <|> pure (fun x -> x) in
  parse ()

(** {1 Utility Functions} *)

(** Bind operator (monadic) for chaining parsers *)
let ( >>= ) (Parser p) f =
  Parser (fun s -> List.concat_map (fun (rest, value) -> run_parser (f value) rest) (p s))

(** Sequence operator (monadic), discarding first result *)
let ( >> ) p1 p2 = p1 >>= fun _ -> p2

(** Choice from a list of parsers *)
let choice parsers = List.fold_left ( <|> ) empty parsers

(** Count: parse exactly n occurrences *)
let rec count n p = if n <= 0 then pure [] else (fun x xs -> x :: xs) <$> p <*> count (n - 1) p

(** Parse one of the given characters *)
let one_of chars = satisfy (fun c -> String.contains chars c)

(** Parse none of the given characters *)
let none_of chars = satisfy (fun c -> not (String.contains chars c))

(** Look ahead - try parser without consuming input *)
let look_ahead (Parser p) =
  Parser (fun s -> match p s with [] -> [] | (_, value) :: _ -> [ (s, value) ])

(** Not followed by - succeeds if parser fails *)
let not_followed_by (Parser p) = Parser (fun s -> match p s with [] -> [ (s, ()) ] | _ -> [])
