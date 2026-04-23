open Core.Syntax
open Core.Utils
open Value_utils

let rec bn = function
  | Var x as v -> v
  | Abs (x, b) as abs -> abs
  | App (m, n) -> apply (bn m) n
and apply m n =
  match m with
    | Abs (x, b) when is_value n ->
      bn @@ subst n x b
    | _ -> App(m, n)
