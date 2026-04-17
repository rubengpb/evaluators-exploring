open Core.Syntax
open Core.Utils

let rec he = function
  | App (t1, t2) ->
    let t1' = he t1 in (
      match t1' with
          | Abs (x, body) -> he @@ subst t2 x body
          | _ -> App (t1', t2)
    )
  | Abs(x, t) -> Abs (x, he t)
  | Var x -> Var x
