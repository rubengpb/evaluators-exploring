open Core.Syntax
open Core.Utils
open Value_utils
open Lv_bv

let rec ha = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, ha b)
  | App (m, n) -> apply (bv m) (ha n)
and apply m n =
  match m with
    | Abs (x, b) ->
      if is_value n
      then ha @@ subst n x b
      else App(ha m, n)
    | _ -> App(ha m, n)
