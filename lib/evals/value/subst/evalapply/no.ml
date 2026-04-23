open Core.Syntax
open Core.Utils
open Value_utils
open Bn

let rec no = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, no b)
  | App (m, n) -> apply (bn m) n
and apply m' n =
  match m' with
    | Abs(x, b) when is_value n ->
      no @@ subst n x b
    | Abs _ -> App(m', n)
    | _ -> App(no m', no n)
