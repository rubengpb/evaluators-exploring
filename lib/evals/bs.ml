open Core.Syntax
open Core.Subst
open Ho

let rec eval_bs = function
  | App (t1, t2) ->
    let t1' = eval_ho t1 in
    (match t1' with
        | Abs (x, body) -> eval_bs @@ subst body x @@ eval_ho t2
        | _ -> App (eval_bs t1', eval_bs t2)
      )

  | Abs(x, t) -> Abs (x, eval_bs t)
  | Var x -> Var x
