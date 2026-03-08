open Core.Syntax
open Core.Subst
open Bn

let rec eval_nor = function
    | App (t1, t2 ) ->
      let t1' = eval_bn t1 in (
        match t1' with
        | Abs(x, body) -> eval_nor @@ subst body x t2
        | _ -> App (eval_nor t1', eval_nor t2)
    )

  | Abs (x, t) ->
      Abs (x, eval_nor t)

  | Var x -> Var x
