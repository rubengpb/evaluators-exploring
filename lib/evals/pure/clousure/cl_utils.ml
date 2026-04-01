open Core.Syntax
open Core.Utils

let rec clean_ctxt x ctxt =
  List.filter (fun (k, _) -> k <> x) ctxt

let rec eval_aux ctxt t =
  match t, ctxt with
  | CVar x, [] -> CVar x
  | CVar x, (y, v') :: c ->
    if x = y then v'
    else eval_aux c (CVar x)
  | CAbs (x, b), c ->
    Clou (CAbs(x, b), c)
  | CApp (t1, t2), cont ->
    apply (eval_aux cont t1) (eval_aux cont t2)
  | _ -> failwith "[Error] Evaluation in expand with Clousure"
  and apply f v =
    match f with
    | Clou (CAbs (x, b), c) -> eval_aux ((x, v)::c) b
    | _ -> CApp (f, v)

let rec pure_of_clousure = function
  | CVar x -> Var x
  | CAbs (x, b) -> Abs (x, pure_of_clousure b)
  | CApp (t1, t2) -> App (pure_of_clousure t1, pure_of_clousure t2)
  | Clou (t, []) -> pure_of_clousure t
  | Clou (t, env) ->
    apply_context env (pure_of_clousure t)
and apply_context env t =
  match env with
  | [] -> t
  | (x, v) :: c ->
    apply_context c (subst (pure_of_clousure (eval_aux [] v)) x t)
