open Core.Syntax

let rec eval_cl_bn ctxt t =
  match t, ctxt with
  | CVar x, [] -> CVar x
  | CVar x, (y, v') :: c ->
    if x = y then v'
    else eval_cl_bn c (CVar x)
  | CAbs (x, b), c ->
    Clou (CAbs(x, b), c)
  | CApp (t1, t2), cont ->
    apply (eval_cl_bn cont t1) t2
  | _ -> failwith "[Error] Evaluation of CallByName with Clousure"
  and apply f v =
    match f with
    | Clou (CAbs (x, b), c) -> eval_cl_bn ((x, v)::c) b
    | _ -> CApp (f, v)
