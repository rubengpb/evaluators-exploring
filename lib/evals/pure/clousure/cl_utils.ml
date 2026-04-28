open Core.Syntax
open Core.Utils

let cl_count = ref 0

let cl_fresh () =
  let old_value = !cl_count in
  cl_count := old_value + 1;
  old_value

let cl_reset () =
  cl_count := 0

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

let rec normalize_var x vars =
  let init = List.hd @@ String.split_on_char '*' x in
  let new_names = List.map snd vars in
  if not (List.mem init new_names) then init
  else new_free_var init new_names

let rec normalize_bound_vars t vars =
  match t, vars with
    | CVar _ as v, [] -> v
    | CVar x, (y, z)::vars ->
      if x = y then CVar z
      else normalize_bound_vars (CVar x) vars
    | CApp(m, n), vars ->
      CApp(normalize_bound_vars m vars, normalize_bound_vars n vars)
    | Clou(t, b), vars ->
      Clou(normalize_bound_vars t vars, normalize_env b vars)
    | CAbs(x, b), vars ->
      let new_x = normalize_var x vars in
      CAbs(new_x, normalize_bound_vars b ((x, new_x)::vars))
  and normalize_env env vars =
    List.map
      (fun (var, body) -> (search_new_name var vars, normalize_bound_vars body vars))
      env
  and search_new_name var vars =
    match vars with
      | [] -> var
      | (x, new_name)::vars ->
        if x = var then new_name
        else search_new_name var vars
