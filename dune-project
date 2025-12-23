(lang dune 3.0)
(name parser_combinator)
(generate_opam_files true)

(source (github aeshabb/parser-combinator))
(license MIT)
(authors "aeshabb")
(maintainers "aeshabb")

(package
 (name parser_combinator)
 (synopsis "Applicative parser combinators in OCaml")
 (description "A functional parser combinator library inspired by Haskell's applicative parsers")
 (depends
  (ocaml (>= 4.14))
  dune
  (alcotest :with-test)
  (ocamlformat :dev)
  (ocp-indent :dev)))
