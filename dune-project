(lang dune 3.19)

(using menhir 2.0)

(name eiron)

(generate_opam_files true)

(source
 (github Bridiro/eiron))

(authors "Alessandro Bridi <ale.bridi15@gmail.com>")

(license LICENSE)

(package
 (name eiron)
 (synopsis "A minimal statically typed language")
 (description "Eiron is a statically typed, compiled language. Designed to be small, fast and educational")
 (depends ocaml menhir dune ppx_deriving)
 (tags
  ("language" "compiler")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
