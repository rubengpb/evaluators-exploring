open Alcotest
open Core.Utils
open Core.Syntax
open Evals.Eval
open Evals.Main_eval

let test_eval_identity () =
  let id = Abs ("x", Var "x") in
  let t = App (id, Var "y") in
  let result = eval (One { language = Pure; style = Apply; strategy = ApplicativeOrder; }) t in
  check string "eval identity" "y" (string_of_term result)

let () =
  run "eval" [
    ("beta", [
      test_case "identity" `Quick test_eval_identity;
    ]);
  ]
