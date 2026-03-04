open Core.Syntax
open Core.Subst

let rec eval_nor = function
  | App (Abs (x, body), arg) ->
      eval_nor (subst body x arg)

  | App (t1, t2) ->
      let t1' = eval_nor t1 in
      if t1' <> t1 then eval_nor (App (t1', t2))
      else App (t1, eval_nor t2)

  | Abs (x, t) ->
      Abs (x, eval_nor t)

  | Var x -> Var x
