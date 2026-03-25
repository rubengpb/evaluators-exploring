open Core.Syntax
open Core.Utils
open Value_utils
open Lv_ho

let rec so = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, so b)
  | App (m, n) -> apply (ho m) (so n)
and apply m n =
  match m with
    | Abs (x, b) ->
      if is_value n
      then so @@ subst n x b
      else App(so m, n)
    | _ -> App(so m, n)
