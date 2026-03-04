open Core.Syntax
open Core.Subst

let rec eval_bn = function
  | App (t1, t2) ->
    let t1' = eval_bn t1 in
    (match t1' with
        | Abs (x, body) -> eval_bn @@ subst body x t2
        | _ -> App (t1', t2)
      )

  | t -> t
