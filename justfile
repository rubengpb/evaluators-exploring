b:
  dune build
t:
  dune test
repl:
  dune build && rlwrap dune exec pure_lambda_repl
main:
  dune build && dune exec evaluators_exploring
