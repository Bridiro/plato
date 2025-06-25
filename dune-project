(lang dune 3.19)

(name plato)

(generate_opam_files true)

(source
 (github Bridiro/plato))

(authors "Alessandro Bridi <ale.bridi15@gmail.com>")

(license LICENSE)

(package
 (name plato)
 (synopsis "A minimal educational language")
 (description "Plato is a statically typed, compiled language. Designed to be small, fast and educational")
 (depends (ocaml (>= 5.1)))
 (tags
  ("language" "compiler")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
