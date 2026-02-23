open Alcotest
open Core
open Core.Syntax
open Core.Subst

let test_simple_var () =
  let t = Var "x" in
  check string "simple variable" "x" (term_to_string t)

let test_simple_application () =
  let t = App (Var "f", Var "x") in
  check string "application" "f x" (term_to_string t)

let test_app_inside_app () =
  let t = App(App(Var "s", Var "z"), Var "s") in
  check string "application" "s s z" (term_to_string t)

let () =
  run "term_to_string" [
    ("printing", [
      test_case "var" `Quick test_simple_var;
      test_case "app" `Quick test_simple_application;
      test_case "app" `Quick test_app_inside_app;
    ]);
  ]
