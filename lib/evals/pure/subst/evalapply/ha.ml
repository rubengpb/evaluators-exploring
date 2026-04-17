open Core.Syntax
open Core.Utils
open Bv

let rec ha = function
  | App (t1, t2) ->
    let t1' = bv t1 in (
      match t1' with
          | Abs (x, body) -> ha @@ subst (ha t2) x body
          | _ -> App (ha t1', ha t2)
    )
  | Abs(x, t) -> Abs (x, ha t)
  | Var x -> Var x
