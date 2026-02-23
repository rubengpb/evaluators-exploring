open Core.Syntax
open Core.Subst
open Core.Strategy

let rec eval_normal = function
  | App (Abs (x, body), arg) ->
      eval_normal (subst body x arg)

  | App (t1, t2) ->
      let t1' = eval_normal t1 in
      if t1' <> t1 then eval_normal (App (t1', t2))
      else App (t1, eval_normal t2)

  | Abs (x, t) ->
      Abs (x, eval_normal t)

  | Var x -> Var x

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

let eval strat t =
  match strat with
  | Normal -> eval_normal t
  | CallByValue -> eval_cbv t
