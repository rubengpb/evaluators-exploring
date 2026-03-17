b:
  dune build
t:
  dune test
repl:
  dune build && dune exec pure_lambda_repl
main:
  dune build && dune exec evaluators_exploring
