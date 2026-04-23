open Core.Syntax
open Core.Utils
open Value_utils

let rec bv = function
  | Var x as v -> v
  | Abs (x, b) as abs -> abs
  | App (m, n) -> apply (bv m) (bv n)
and apply m' n' =
  match m' with
    | Abs(x, b) when is_value n' ->
      bv @@ subst n' x b
    | _ -> App(m', n')
