open Core.Syntax
open Core.Utils
open Value_utils

let rec bn = function
  | Var x as v -> v
  | Abs (x, b) as abs -> abs
  | App (m, n) -> apply (bn m) n
and apply m n =
  match m with
    | Abs (x, b) ->
      if is_value n
      then bn @@ subst n x b
      else App(m, n)
    | _ -> App(m, n)
