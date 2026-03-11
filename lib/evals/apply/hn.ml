open Core.Syntax
open Core.Utils
open He

let rec eval_hn = function
  | App (t1, t2) ->
    let t1' = eval_he t1 in (
      match t1' with
          | Abs (x, body) -> eval_he @@ subst t2 x body
          | _ -> App (eval_he t1', eval_he t2)
    )
  | Abs(x, t) -> Abs (x, eval_hn t)
  | Var x -> Var x
