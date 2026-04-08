open Core.Syntax
open Core.Utils

let rec bn = function
  | App (t1, t2) ->
    let t1' = bn t1 in (
      match t1' with
          | Abs (x, body) -> bn @@ subst t2 x body
          | _ -> App (t1', t2)
    )
  | t -> t
