open Core.Syntax
open Core.Utils
open Value_utils
open Lv_he

let rec hn = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, hn b)
  | App (m, n) -> apply (he m) n
and apply m n =
  match m with
    | Abs (x, b) ->
      if is_value n
      then hn @@ subst n x b
      else App(hn m, hn n)
    | _ -> App(hn m, hn n)
