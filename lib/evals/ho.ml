open Core.Syntax
open Core.Subst

let rec eval_ho = function
  | App (t1, t2) ->
    let t1' = eval_ho t1 in
    (match t1' with
        | Abs (x, body) -> eval_ho @@ subst body x @@ eval_ho t2
        | _ -> App (t1', t2)
      )

  | Abs(x, t) -> Abs (x, eval_ho t)
  | Var x -> Var x
