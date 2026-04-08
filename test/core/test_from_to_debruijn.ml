open Alcotest
open Core.Syntax
open Core.Utils

let test_simple_var () =
  let t = Var "x" in
  let dbt = dbterm_of_pterm t in
  check string "simple variable" "x" (string_of_dbterm_pure dbt)

let test_simple_abs () =
  let t = Abs("x", Var "y") in
  let dbt = dbterm_of_pterm t in
  check bool "simple abstraction" true (alpha_equiv t @@ pterm_of_dbterm dbt);
  check string "simple abstraction" "\\.y" (string_of_dbterm_pure dbt)

let test_simple_application () =
  let t = App (Var "f", Var "x") in
  let dbt = dbterm_of_pterm t in
  check bool "application" true (alpha_equiv t @@ pterm_of_dbterm dbt);
  check string "application" "f x" (string_of_dbterm_pure dbt)

let test_application_var_abs () =
  let t = App (Var "f", Abs("x", Var "x")) in
  let dbt = dbterm_of_pterm t in
  check bool "application" true (alpha_equiv t @@ pterm_of_dbterm dbt);
  check string "application" "f (\\.0)" (string_of_dbterm_pure dbt)

let test_two_lambda_abs_true () =
  let t = Abs("a", Abs("b", Var "a")) in
  let dbt = dbterm_of_pterm t in
  check bool "true abs" true (alpha_equiv t @@ pterm_of_dbterm dbt);
  check string "true abs" "\\.\\.1" (string_of_dbterm_pure dbt)

let test_two_lambda_abs_false () =
  let t = Abs("a", Abs("b", Var "b")) in
  let dbt = dbterm_of_pterm t in
  check bool "true abs" true (alpha_equiv t @@ pterm_of_dbterm dbt);
  check string "true abs" "\\.\\.0" (string_of_dbterm_pure dbt)

let test_application_var_abs2 () =
  let t = App(Var "s", Abs("s", Var "z")) in
  let dbt = dbterm_of_pterm t in
  check bool "application" true (alpha_equiv t @@ pterm_of_dbterm dbt);
  check string "application" "s (\\.z)" (string_of_dbterm_pure dbt)

let test_application_abs_app () =
  let t = App(Abs("s", Var "x"), App(Var "s", Var "z")) in
  let dbt = dbterm_of_pterm t in
  check bool "application" true (alpha_equiv t @@ pterm_of_dbterm dbt);
  check string "application" "(\\.x) (s z)" (string_of_dbterm_pure dbt)

let test_application_abs_var () =
  let t = App(Abs("s", Var "s"), Abs("s", Var "s")) in
  let dbt = dbterm_of_pterm t in
  check bool "application" true (alpha_equiv t @@ pterm_of_dbterm dbt);
  check string "application" "(\\.0) (\\.0)" (string_of_dbterm_pure dbt)

let () =
  run "dbterm" [
    ("printing", [
      test_case "var" `Quick test_simple_var;
      test_case "abs" `Quick test_simple_abs;
      test_case "app" `Quick test_simple_application;
      test_case "app var with abs" `Quick test_application_var_abs;
      test_case "two lambda, equiv true" `Quick test_two_lambda_abs_true;
      test_case "two lambda, equiv false" `Quick test_two_lambda_abs_false;
      test_case "app var with abs" `Quick test_application_var_abs2;
      test_case "app abs app" `Quick test_application_abs_app;
      test_case "app abs var" `Quick test_application_abs_var;
    ]);
  ]
