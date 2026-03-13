open Alcotest
open Core.Syntax
open Core.Utils

let test_same_variable () =
  check bool "same variable"
    true
    (alpha_equiv (Var "x") (Var "x"))

let test_different_variable () =
  check bool "different variables"
    false
    (alpha_equiv (Var "x") (Var "y"))

let test_simple_alpha_equiv () =
  check bool "\\x.x = \\y.y"
    true
    (alpha_equiv
       (Abs ("x", Var "x"))
       (Abs ("y", Var "y")))

let test_not_alpha_equiv () =
  check bool "\\x.x != \\x.y"
    false
    (alpha_equiv
       (Abs ("x", Var "x"))
       (Abs ("x", Var "y")))

let test_application_alpha_equiv () =
  check bool "(\\x.x) y = (\\z.z) y"
    true
    (alpha_equiv
       (App (Abs ("x", Var "x"), Var "y"))
       (App (Abs ("z", Var "z"), Var "y")))

let test_application_not_alpha_equiv () =
  check bool "(\\x.x) y != (\\x.x) z"
    false
    (alpha_equiv
       (App (Abs ("x", Var "x"), Var "y"))
       (App (Abs ("x", Var "x"), Var "z")))

let test_nested_alpha_equiv () =
  check bool "\\x.\\y.x = \\a.\\b.a"
    true
    (alpha_equiv
       (Abs ("x", Abs ("y", Var "x")))
       (Abs ("a", Abs ("b", Var "a"))))

let test_nested_not_alpha_equiv () =
  check bool "\\x.\\y.x != \\a.\\b.b"
    false
    (alpha_equiv
       (Abs ("x", Abs ("y", Var "x")))
       (Abs ("a", Abs ("b", Var "b"))))

let () =
  run "alpha_equiv tests"
    [
      ("variables",
       [
         test_case "same variable" `Quick test_same_variable;
         test_case "different variable" `Quick test_different_variable;
       ]);

      ("abstractions",
       [
         test_case "simple alpha equiv" `Quick test_simple_alpha_equiv;
         test_case "not alpha equiv" `Quick test_not_alpha_equiv;
         test_case "nested alpha equiv" `Quick test_nested_alpha_equiv;
         test_case "nested not alpha equiv" `Quick test_nested_not_alpha_equiv;
       ]);

      ("applications",
       [
         test_case "application alpha equiv" `Quick test_application_alpha_equiv;
         test_case "application not alpha equiv" `Quick test_application_not_alpha_equiv;
       ]);
    ]
