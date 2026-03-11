open Core.Syntax
open Core.Utils
open Ho

let rec eval_so = function
  | App (t1, t2) ->
    let t1' = eval_ho t1 in (
      match t1' with
          | Abs (x, body) -> eval_so @@ subst (eval_so t2) x body
          | _ -> App (eval_so t1', eval_so t2)
    )
  | Abs(x, t) -> Abs (x, eval_so t)
  | Var x -> Var x
