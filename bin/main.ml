open Core.Syntax
open Core.Strategy
open Core.Utils
open Evals.Main_eval

let id =
  Abs ("x", Var "x")

let k =
  Abs ("x", Abs ("y", Var"x"))

let s =
  Abs ("f", Abs ("g", Abs ("x", App(App(Var "f", Var "x"), App(Var "g", Var "x")))))

let b =
  Abs ("f", Abs("g", Abs("x", App(Var "f", App(Var "g", Var "x")))))

let omega_aux =
  Abs ("x", App(Var "x", Var "x"))

let omega =
  App (omega_aux, omega_aux)

let once =
  Abs ("s", Abs("x", App(Var "s", Var "x")))

let twice =
  Abs ("s", Abs("x", App(Var "s", App(Var "s", Var "x"))))

let tests =
  [
    ("id id", App(id, id));
    ("\\x. id id", Abs("x", App(id, id)));
    ("\\x. id id y", Abs("x", App(App (id, id), Var "y")));
    ("(\\x. id id) y", App(Abs("x", App (id, id)), Var "y"));
    ("K a b", App(App(k, Var "a"), Var "b"));
    ("twice id y", App(App(twice, id), Var "y"));
    ("K id omega", App(App(k, id), omega));
  ]


let () =
  print_endline "===============";
  print_endline "SN:";
  print_endline "===============";
  List.iter
    (fun (name, term) ->
      print_endline ("Test: " ^ name);
      print_endline ("  input : " ^ term_to_string term);
      let result = eval HybridNormalOrder term in
      print_endline ("  result: " ^ term_to_string result);
      print_endline ""
    )
    tests;
