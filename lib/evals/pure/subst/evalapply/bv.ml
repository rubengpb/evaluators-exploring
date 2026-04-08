open Core.Syntax
open Core.Utils

let rec bv = function
  | App (t1, t2) ->
    let t1' = bv t1 in (
      match t1' with
          | Abs (x, body) -> bv @@ subst (bv t2) x body
          | _ -> App (t1', bv t2)
    )
  | t -> t
