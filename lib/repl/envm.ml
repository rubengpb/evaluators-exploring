open Core.Syntax

let rec expand env = function
  | Var x ->
      (match List.assoc_opt x env with
       | Some t -> t
       | None -> Var x)

  | Abs (x,t) ->
      Abs (x, expand (List.remove_assoc x env) t)

  | App (t1,t2) ->
      App (expand env t1, expand env t2)
