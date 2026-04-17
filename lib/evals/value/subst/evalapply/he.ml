open Core.Syntax
open Core.Utils
open Value_utils

let rec he = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, he b)
  | App (m, n) -> apply (he m) n
and apply m n =
  match m with
    | Abs (x, b) ->
      if is_value n
      then he @@ subst n x b
      else App(m, n)
    | _ -> App(m, n)
