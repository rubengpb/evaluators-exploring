open Core.Syntax
open Core.Utils
open Value_utils
open Lv_bn

let rec no = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, no b)
  | App (m, n) -> apply (bn m) n
and apply m n =
  match m with
    | Abs (x, b) ->
      if is_value n
      then no @@ subst n x b
      else App(no m, no n)
    | _ -> App(no m, no n)
