open Alcotest
open Core.Syntax
open Core.Utils

let test_simple_var () =
  let t = Var "x" in
  check string "simple variable" "x" (term_to_string t)

let test_simple_abs () =
  let t = Abs("x", Var "y") in
  check string "simple abstraction" "\\x.y" (term_to_string t)

let test_simple_application () =
  let t = App (Var "f", Var "x") in
  check string "application" "f x" (term_to_string t)

let test_app_inside_app_left () =
  let t = App(App(Var "s", Var "z"), Var "s") in
  check string "application" "s s z" (term_to_string t)

let test_app_inside_app_right () =
  let t = App(Var "s", App(Var "s", Var "z")) in
  check string "application" "s (s z)" (term_to_string t)

let () =
  run "term_to_string" [
    ("printing", [
      test_case "var" `Quick test_simple_var;
      test_case "abs" `Quick test_simple_abs;
      test_case "app" `Quick test_simple_application;
      test_case "app 3 vars left" `Quick test_app_inside_app_left;
      test_case "app 3 vars rigth" `Quick test_app_inside_app_right;
    ]);
  ]
