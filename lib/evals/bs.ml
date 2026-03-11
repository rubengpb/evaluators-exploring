open Core.Syntax
open Core.Utils
open Ho

let rec eval_bs = function
  | App (t1, t2) ->
    let t1' = eval_ho t1 in (
      match t1' with
          | Abs (x, body) -> eval_bs @@ subst (eval_ho t2) x body
          | _ -> App (eval_bs t1', eval_bs t2)
    )
  | Abs(x, t) -> Abs (x, eval_bs t)
  | Var x -> Var x
