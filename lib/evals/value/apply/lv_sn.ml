open Core.Syntax
open Core.Utils
open Value_utils
open Lv_bv

let rec sn = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, sn b)
  | App (m, n) -> apply (bv m) (bv n)
and apply m n =
  match m with
    | Abs (x, b) ->
      if is_value n
      then sn @@ subst n x b
      else App(sn m, sn n)
    | _ -> App(sn m, sn n)
