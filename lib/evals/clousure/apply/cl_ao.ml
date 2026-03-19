open Core.Syntax
open Cl_utils

let rec eval_cl_ao ctxt t =
  match t, ctxt with
  | CVar x, [] -> CVar x
  | CVar x, (y, v') :: c ->
    if x = y then v'
    else eval_cl_ao c (CVar x)
  | CAbs (x, b), c ->
    Clou (CAbs(x, eval_cl_ao [] b), c)
  | CApp (t1, t2), cont ->
    apply (eval_cl_ao cont t1) (eval_cl_ao cont t2)
  | Clou(_, _), _ -> t
  and apply f v =
    match f with
    | Clou (CAbs (x, b), c) -> eval_cl_ao ((x, v)::c) b
    | _ -> CApp (f, v)
