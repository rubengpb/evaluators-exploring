open Core.Syntax
open Core.Utils
open Bv

let rec am = function
  | App (t1, t2) ->
    let t1' = bv t1 in (
      match t1' with
          | Abs (x, body) -> am @@ subst (bv t2) x body
          | _ -> App (am t1', bv t2)
    )
  | Abs(x, t) -> Abs (x, am t)
  | Var x -> Var x
