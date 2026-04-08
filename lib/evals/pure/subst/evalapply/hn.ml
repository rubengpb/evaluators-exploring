open Core.Syntax
open Core.Utils
open He

let rec hn = function
  | App (t1, t2) ->
    let t1' = he t1 in (
      match t1' with
          | Abs (x, body) -> hn @@ subst t2 x body
          | _ -> App (hn t1', hn t2)
    )
  | Abs(x, t) -> Abs (x, hn t)
  | Var x -> Var x
