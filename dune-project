(lang dune 3.19)

(using menhir 2.0)

(name plato)

(generate_opam_files true)

(source
 (github Bridiro/plato))

(authors "Alessandro Bridi <ale.bridi15@gmail.com>")

(license LICENSE)

(package
 (name plato)
 (synopsis "A minimal statically typed language")
 (description "Plato is a statically typed, compiled language. Designed to be small, fast and educational")
 (depends ocaml menhir dune ppx_deriving)
 (tags
  ("language" "compiler")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
