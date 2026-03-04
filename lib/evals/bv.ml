open Core.Syntax
open Core.Subst

let rec eval_cbv = function
  | App (Abs (x, body), arg) when is_value arg ->
      eval_cbv (subst body x arg)

  | App (t1, t2) when not (is_value t1) ->
      App (eval_cbv t1, t2)

  | App (t1, t2) ->
      App (t1, eval_cbv t2)

  | t -> t

and is_value = function
  | Abs _ -> true
  | _ -> false
