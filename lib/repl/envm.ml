open Core.Syntax

let rec expand_aux env = function
  | Var x ->
      (match List.assoc_opt x env with
       | Some t -> t
       | None -> Var x)

  | Abs (x,t) ->
      Abs (x, expand_aux (List.remove_assoc x env) t)

  | App (t1,t2) ->
      App (expand_aux env t1, expand_aux env t2)

let expand env t = expand_aux (List.map (fun (v,t,_) -> (v,t)) env) t
