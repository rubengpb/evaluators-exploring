open Core.Syntax
open Core.Utils
open Bn

let rec eval_no = function
  | App (t1, t2 ) ->
    let t1' = eval_bn t1 in (
      match t1' with
      | Abs(x, body) -> eval_no @@ subst t2 x body
      | _ -> App (eval_no t1', eval_no t2)
    )
  | Abs (x, t) -> Abs (x, eval_no t)
  | Var x -> Var x
