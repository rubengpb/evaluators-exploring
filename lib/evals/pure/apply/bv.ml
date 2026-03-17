open Core.Syntax
open Core.Utils

let rec eval_bv = function
  | App (t1, t2) ->
    let t1' = eval_bv t1 in (
      match t1' with
          | Abs (x, body) -> eval_bv @@ subst (eval_bv t2) x body
          | _ -> App (t1', eval_bv t2)
    )
  | t -> t
