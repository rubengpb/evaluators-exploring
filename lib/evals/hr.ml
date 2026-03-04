open Core.Syntax
open Core.Subst
open Bn

let rec eval_hr = function
  | App (t1, t2) ->
    let t1' = eval_bn t1 in
    (match t1' with
        | Abs (x, body) -> eval_hr @@ subst body x t2
        | _ -> App (eval_hr t1', t2)
      )

  | Abs(x, t) -> Abs (x, eval_hr t)
  | Var x -> Var x
