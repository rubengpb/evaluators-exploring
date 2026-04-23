open Core.Syntax
open Core.Utils
open Value_utils

let rec ao = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, ao b)
  | App (m, n) -> apply (ao m) (ao n)
and apply m n =
  match m with
    | Abs (x, b) when is_value n ->
      ao @@ subst n x b
    | _ -> App(m, n)
