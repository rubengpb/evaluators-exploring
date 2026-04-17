open Core.Syntax
open Core.Utils
open Bn

let rec no = function
  | App (t1, t2 ) ->
    let t1' = bn t1 in (
      match t1' with
      | Abs(x, body) -> no @@ subst t2 x body
      | _ -> App (no t1', no t2)
    )
  | Abs (x, t) -> Abs (x, no t)
  | Var x -> Var x
