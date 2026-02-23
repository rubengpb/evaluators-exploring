open Alcotest
open Core
open Core.Syntax
open Core.Subst

let test_simple_subst () =
  let t = Var "z" in
  let result = subst (Var "y") "y" t in
  check string "subst y -> z" "z" (term_to_string result)

let () =
  run "subst" [
    ("basic", [
      test_case "simple" `Quick test_simple_subst;
    ]);
  ]
