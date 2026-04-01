open Core.Syntax

let rec bv ctxt t =
  match t, ctxt with
  | CVar x, [] -> CVar x
  | CVar x, (y, v') :: c ->
    if x = y then v'
    else bv c (CVar x)
  | CAbs (x, b), c ->
    Clou (CAbs(x, b), c)
  | CApp (t1, t2), cont ->
    apply (bv cont t1) (bv cont t2)
  (* | Clou (t , c1), c2 -> bv (c1 @ c2) t *)
  | _ -> failwith "[Error] Evaluation of CallByValue with Clousure"
  and apply f v =
    match f with
    | Clou (CAbs (x, b), c) -> bv ((x, v)::c) b
    | _ -> CApp (f, v)
