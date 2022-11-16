(lang dune 3.2)

(using menhir 2.1)

(generate_opam_files true)

(package
 (name trivia-parser)
 (depends
   ocaml
   dune
   menhir
   ppx_import
   ppx_deriving
   printbox
   printbox-text
  (ppx_expect :with-test))
 (allow_empty))
