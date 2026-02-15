open Evaluators_exploring.Syntax
open Evaluators_exploring.Eval_subst
open Evaluators_exploring.Strategy

let id =
  Abs ("x", Var "x")

let term =
  App (id, id)

let rec term_to_string t =
  match t with
  | Var x -> x
  | Abs (x, t) -> "\\" ^ x ^ "." ^ term_to_string t
  | App (t1, t2) -> "(" ^ term_to_string t1 ^ term_to_string t2 ^ ")"


let () =
  let result = eval Normal term in
  print_endline (term_to_string result)
