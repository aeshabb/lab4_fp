(** Applicative Parser Combinator Library

    This module provides a functional parser combinator library inspired by Haskell's applicative
    parsers. *)

(** {1 Core Types} *)

type 'a t
(** A parser for values of type ['a]. *)

val run_parser : 'a t -> string -> (string * 'a) list
(** Unwrap the parser function *)

val parse_string : 'a t -> string -> 'a option
(** Try to parse a complete string. Returns [Some value] if parsing succeeds with no remaining
    input, [None] otherwise. *)

val parse_all : 'a t -> string -> (string * 'a) list
(** Parse a string and return all possible results *)

(** {1 Basic Parsers} *)

val satisfy : (char -> bool) -> char t
(** Parser that accepts a single character satisfying a predicate *)

val pred_p : (char -> bool) -> char t
(** Alias for [satisfy] *)

val char_p : char -> char t
(** Parser for a specific character *)

val empty : 'a t
(** Parser that always fails *)

val pure : 'a -> 'a t
(** Parser that succeeds without consuming input, returning the given value *)

val lazy_p : (unit -> 'a t) -> 'a t
(** Lazy parser wrapper to enable recursion without stack overflow *)

val eof : unit t
(** Parser that returns unit if at the end of input *)

(** {1 Functor Operations} *)

val fmap : ('a -> 'b) -> 'a t -> 'b t
(** Map a function over the result of a parser *)

val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
(** Infix operator for [fmap]: [f <$> p] is equivalent to [fmap f p] *)

val ( <$ ) : 'a -> 'b t -> 'a t
(** Replace the result with a constant value *)

val ( $> ) : 'a t -> 'b -> 'b t
(** Replace the result with a constant value (flipped) *)

(** {1 Applicative Operations} *)

val apply : ('a -> 'b) t -> 'a t -> 'b t
(** Apply a parser containing a function to a parser containing a value *)

val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
(** Infix operator for [apply] *)

val ( *> ) : 'a t -> 'b t -> 'b t
(** Sequence two parsers, keeping only the result of the second *)

val ( <* ) : 'a t -> 'b t -> 'a t
(** Sequence two parsers, keeping only the result of the first *)

val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Lift a binary function to work on parsers *)

val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
(** Lift a ternary function to work on parsers *)

(** {1 Alternative Operations} *)

val alt : 'a t -> 'a t -> 'a t
(** Try first parser, if it fails try second parser *)

val ( <|> ) : 'a t -> 'a t -> 'a t
(** Infix operator for [alt] *)

val many : 'a t -> 'a list t
(** Parse zero or more occurrences *)

val some : 'a t -> 'a list t
(** Parse one or more occurrences *)

val optional : 'a t -> 'a option t
(** Optional parser - returns [Some x] if succeeds, [None] otherwise *)

(** {1 String Parsers} *)

val string_p : string -> string t
(** Parser for a specific string prefix *)

val skip_string : string -> unit t
(** Parser for a string prefix, returns unit *)

val skip_while : (char -> bool) -> unit t
(** Skip characters while predicate holds *)

val take_while : (char -> bool) -> string t
(** Take characters while predicate holds *)

val take_while1 : (char -> bool) -> string t
(** Take at least one character while predicate holds *)

val skip_spaces : unit t
(** Skip whitespace characters *)

(** {1 Character Class Parsers} *)

val digit : char t
(** Parse a digit character *)

val letter : char t
(** Parse a letter character *)

val alphanum : char t
(** Parse an alphanumeric character *)

(** {1 Numeric Parsers} *)

val natural : int t
(** Parse a natural number (non-negative integer) *)

val integer : int t
(** Parse an integer (optionally negative) *)

(** {1 Combinators} *)

val between : 'a t -> 'b t -> 'c t -> 'c t
(** Parse something between two other parsers *)

val sep_by : 'a t -> 'b t -> 'b list t
(** Parse items separated by a separator *)

val sep_by1 : 'a t -> 'b t -> 'b list t
(** Parse items separated by a separator (at least one item) *)

val chainl1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t
(** Chain left-associative binary operations *)

val chainr1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t
(** Chain right-associative binary operations *)

(** {1 Utility Functions} *)

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
(** Bind operator (monadic) for chaining parsers *)

val ( >> ) : 'a t -> 'b t -> 'b t
(** Sequence operator (monadic), discarding first result *)

val choice : 'a t list -> 'a t
(** Choice from a list of parsers *)

val count : int -> 'a t -> 'a list t
(** Count: parse exactly n occurrences *)

val one_of : string -> char t
(** Parse one of the given characters *)

val none_of : string -> char t
(** Parse none of the given characters *)

val look_ahead : 'a t -> 'a t
(** Look ahead - try parser without consuming input *)

val not_followed_by : 'a t -> unit t
(** Not followed by - succeeds if parser fails *)
