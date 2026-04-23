open Core.Syntax
open Core.Utils
open Value_utils

let rec he = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, he b)
  | App (m, n) -> apply (he m) n
and apply m n =
  match m with
    | Abs (x, b) when is_value n ->
      he @@ subst n x b
    | _ -> App(m, n)
