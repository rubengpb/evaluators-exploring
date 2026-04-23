open Core.Syntax
open Core.Utils
open Value_utils
open Bv

let rec am = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, am b)
  | App (m, n) -> apply (bv m) (bv n)
and apply m' n =
  match m' with
    | Abs (x, b) ->
      let n' = bv n in
      if is_value n' then am @@ subst n' x b
      else App(m', n')
    | _ -> App(am m', bv n)
