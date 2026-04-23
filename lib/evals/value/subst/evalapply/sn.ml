open Core.Syntax
open Core.Utils
open Value_utils
open Bv

let rec sn = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, sn b)
  | App (m, n) -> apply (bv m) n
and apply m' n =
  match m' with
    | Abs (x, b) ->
      let n' = bv n in
      if is_value n' then sn @@ subst n' x b
      else App(m', n')
    | _ -> App(sn m', sn n)
