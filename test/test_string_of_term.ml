open Alcotest
open Core.Syntax
open Core.Utils

let test_simple_var () =
  let t = Var "x" in
  check string "simple variable" "x" (string_of_term t)

let test_simple_abs () =
  let t = Abs("x", Var "y") in
  check string "simple abstraction" "\\x.y" (string_of_term t)

let test_simple_application () =
  let t = App (Var "f", Var "x") in
  check string "application" "f x" (string_of_term t)

let test_application_var_abs () =
  let t = App (Var "f", Abs("x", Var "x")) in
  check string "application" "f (\\x.x)" (string_of_term t)

let test_application_app_var () =
  let t = App(App(Var "s", Var "z"), Var "s") in
  check string "application" "s z s" (string_of_term t)

let test_application_var_abs () =
  let t = App(Var "s", Abs("s", Var "z")) in
  check string "application" "s (\\s.z)" (string_of_term t)

let test_application_var_app () =
  let t = App(Var "s", App(Var "s", Var "z")) in
  check string "application" "s (s z)" (string_of_term t)

let test_application_abs_app () =
  let t = App(Abs("s", Var "x"), App(Var "s", Var "z")) in
  check string "application" "(\\s.x) (s z)" (string_of_term t)

let test_application_abs_var () =
  let t = App(Abs("s", Var "x"), Var "s") in
  check string "application" "(\\s.x) s" (string_of_term t)

let test_application_app_app () =
  let t = App(App(Var "s", Var "x"), App(Var "s", Var "x")) in
  check string "application" "s x (s x)" (string_of_term t)

let () =
  run "string_of_term" [
    ("printing", [
      test_case "var" `Quick test_simple_var;
      test_case "abs" `Quick test_simple_abs;
      test_case "app" `Quick test_simple_application;
      test_case "app 3 vars left" `Quick test_application_app_var;
      test_case "app var with abs" `Quick test_application_var_abs;
      test_case "app 3 vars rigth" `Quick test_application_var_app;
      test_case "app abs app" `Quick test_application_abs_app;
      test_case "app abs var" `Quick test_application_abs_var;
      test_case "app app app" `Quick test_application_app_app;
    ]);
  ]
