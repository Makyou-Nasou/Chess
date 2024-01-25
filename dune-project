(lang dune 3.12)

(name Chess)

(generate_opam_files true)

(source
 (uri git+https://github.com/Makyou-Nasou/Echecs))

(authors  "Thomas Arrous")

(maintainers  "Thomas Arrous")

(license LICENSE)

(package
 (name Chess)
 (synopsis "Chess board game")
 (description "Reproduction of the chess game.")
 (depends ocaml dune (odoc :with-doc) ppx_deriving (alcotest :with-test) js_of_ocaml js_of_ocaml-lwt (qcheck :with-test) (qcheck-alcotest :with-test))
 (tags
  ("board game" "chess" "ocaml project")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
