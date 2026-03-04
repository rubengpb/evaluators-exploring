open Core.Syntax
open Core.Subst
open Bv

let rec eval_ha = function
  | App (t1, t2) ->
    let t1' = eval_bv t1 in
    (match t1' with
        | Abs (x, body) -> eval_ha @@ subst body x @@ eval_ha t2
        | _ -> App (eval_ha t1', eval_ha t2)
      )

  | Abs(x, t) -> Abs (x, eval_ha t)
  | Var x -> Var x
