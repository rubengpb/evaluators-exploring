open Core.Syntax
open Core.Utils
open Value_utils

let rec bv = function
  | Var x as v -> v
  | Abs (x, b) as abs -> abs
  | App (m, n) -> apply (bv m) (bv n)
and apply m n =
  match m with
    | Abs (x, b) ->
      if is_value n
      then bv @@ subst n x b
      else App(m, n)
    | _ -> App(m, n)
