open Alcotest
open Core.Subst
open Core.Syntax
open Core.Strategy
open Evals.Main_eval

let test_eval_identity () =
  let id = Abs ("x", Var "x") in
  let t = App (id, Var "y") in
  let result = eval Normal t in
  check string "eval identity" "y" (Core.Subst.term_to_string result)

let () =
  run "eval" [
    ("beta", [
      test_case "identity" `Quick test_eval_identity;
    ]);
  ]
