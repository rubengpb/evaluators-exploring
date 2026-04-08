open Core.Syntax
open Core.Utils
open Bv

let rec sn = function
  | App (t1, t2) ->
    let t1' = bv t1 in (
      match t1' with
          | Abs (x, body) -> sn @@ subst (bv t2) x body
          | _ -> App (sn t1', sn t2)
    )
  | Abs(x, t) -> Abs (x, sn t)
  | Var x -> Var x
