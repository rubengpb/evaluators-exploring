open Core.Syntax
open Cl_utils
open Cl_bn

let rec eval_cl_no ctxt t =
  match t, ctxt with
  | CVar x, [] -> CVar x
  | CVar x, (y, v') :: c ->
    if x = y then eval_cl_no c v'
    else eval_cl_no c (CVar x)
  | CAbs (x, b), c ->
    Clou (CAbs(x, eval_cl_no [] b), c)
  | CApp (t1, t2), cont ->
    apply cont (eval_cl_bn cont t1) t2
  | Clou(_, _), _ -> t
  and apply ctxt f v =
    match f with
    | Clou (CAbs (x, b), c) -> eval_cl_no ((x, v)::c) b
    | _ -> CApp (eval_cl_no ctxt f, eval_cl_no ctxt v)
