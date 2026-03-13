open Core.Syntax
open Core.Utils

let rec eval_bn = function
  | App (t1, t2) ->
    let t1' = eval_bn t1 in (
      match t1' with
          | Abs (x, body) -> eval_bn @@ subst t2 x body
          | _ -> App (t1', t2)
    )
  | t -> t
