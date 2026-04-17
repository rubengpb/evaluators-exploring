# Evaluators exploring

This repo contains the first proofs of implementing *lambda-evaluators*

## Run

```bash
dune build
dune exec evaluators-exploring
dune clean
```

## Test

Test run with `alcotest`.

Install with:

```bash
opam install alcotest
```

Execute with:

```bash
dune test
dune test test_eval
```

## repl

From the system, install `rlwrap` if you want to have a better experience
typing inside the repl.

The dependencies:

```bash
opam install menhir
```
