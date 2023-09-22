(lang dune 3.8)

(name expression)

(generate_opam_files true)

(source
 (github Merlin04/pldi-expression))

(license LICENSE)

(package
 (name expression)
 (synopsis "PLDI expression")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

(using menhir 2.1)