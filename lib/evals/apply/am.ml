open Core.Syntax
open Core.Utils
open Bv

let rec eval_am = function
  | App (t1, t2) ->
    let t1' = eval_bv t1 in (
      match t1' with
          | Abs (x, body) -> eval_am @@ subst (eval_bv t2) x body
          | _ -> App (eval_am t1', eval_bv t2)
    )
  | Abs(x, t) -> Abs (x, eval_am t)
  | Var x -> Var x
