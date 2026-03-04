open Core.Syntax
open Core.Strategy
open Core.Subst
open Evals.Main_eval

let id =
  Abs ("x", Var "x")

let k =
  Abs ("x", Abs ("y", Var"x"))

let s =
  Abs ("f", Abs ("g", Abs ("x", App(App(Var "f", Var "x"), App(Var "g", Var "x")))))

let b =
  Abs ("f", Abs("g", Abs("x", App(Var "f", App(Var "g", Var "x")))))

let omega =
  Abs ("x", App(Var "x", Var "x"))

let once =
  Abs ("s", Abs("x", App(Var "s", Var "x")))

let twice =
  Abs ("s", Abs("x", App(Var "s", App(Var "s", Var "x"))))

let tests =
  [
    ("id id", App(id, id));
    ("\\x. id id", Abs("x", App(id, id)));
    ("K a b", App(App(k, Var "a"), Var "b"));
    ("twice id y", App(App(twice, id), Var "y"));
    (* ("K id omega", App(App(k, id), omega)); *)
  ]


let () =
  print_endline "===============";
  print_endline "NOR:";
  print_endline "===============";
  List.iter
    (fun (name, term) ->
      print_endline ("Test: " ^ name);
      print_endline ("  input : " ^ term_to_string term);
      let result = eval Normal term in
      print_endline ("  result: " ^ term_to_string result);
      print_endline ""
    )
    tests;
  print_endline "===============";
  print_endline "BV:";
  print_endline "===============";
  List.iter
    (fun (name, term) ->
      print_endline ("Test: " ^ name);
      print_endline ("  input : " ^ term_to_string term);
      let result = eval CallByValue term in
      print_endline ("  result: " ^ term_to_string result);
      print_endline ""
    )
    tests;
  print_endline "===============";
  print_endline "AO:";
  print_endline "===============";
  List.iter
    (fun (name, term) ->
      print_endline ("Test: " ^ name);
      print_endline ("  input : " ^ term_to_string term);
      let result = eval ApplicativeOrder term in
      print_endline ("  result: " ^ term_to_string result);
      print_endline ""
    )
    tests;
