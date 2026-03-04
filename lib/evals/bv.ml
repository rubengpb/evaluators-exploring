open Core.Syntax
open Core.Subst

let rec eval_bv = function
  | App (t1, t2) ->
    let t1' = eval_bv t1 in
    (match t1' with
        | Abs (x, body) -> eval_bv @@ subst body x @@ eval_bv t2
        | _ -> App (t1', eval_bv t2)
      )

  | t -> t
