open Core.Syntax
open Core.Utils
open Value_utils
open Bn

let rec hr = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, hr b)
  | App (m, n) -> apply (bn m) n
and apply m n =
  match m with
    | Abs (x, b) ->
      if is_value n
      then hr @@ subst n x b
      else App(hr m, n)
    | _ -> App(hr m, n)
