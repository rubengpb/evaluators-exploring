open Alcotest
open Core
open Core.Syntax
open Core.Utils

let test_simple_subst () =
  let t = Var "z" in
  let result = subst (Var "y") "y" t in
  check string "subst y -> z" "z" (term_to_string result)

let test_simple_subst_dif () =
  let t = Var "z" in
  let result = subst (Var "y") "y" t in
  check string "subst y -> z" "z" (term_to_string result)

let () =
  run "subst" [
    ("basic", [
      test_case "one variable" `Quick test_simple_subst;
      test_case "one diferent variable" `Quick test_simple_subst_dif;
    ]);
  ]
