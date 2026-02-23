(lang dune 3.21)

(name evaluators_exploring)

(generate_opam_files true)

(source
 (github helarteDeProgrmar/evaluators-exploring))

(authors "Rubén García-Patos Benito <ruben.gbenito@alumnos.upm.es>")

(maintainers "Rubén García-Patos Benito <ruben.gbenito@alumnos.upm.es>")

; (license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name evaluators_exploring)
 (synopsis "Exploring the strategy space of the lambda-calculus evaluators")
 (description "Project growing while it is being studied the call-by-value strategy")
 (depends ocaml)
 (tags
  ("lambda-calculus" "call-by-value")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
