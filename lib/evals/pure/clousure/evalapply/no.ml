open Core.Syntax
open Core.Utils
open Cl_utils
open Bn

(* let rec eval_cl_no ctxt t = *)
(*   match t, ctxt with *)
(*   | CVar x, [] -> CVar x *)
(*   | CVar x, (y, v') :: c -> *)
(*     if x = y then eval_cl_no c v' *)
(*     else eval_cl_no c (CVar x) *)
(*   | CAbs (x, b), c -> *)
(*     Clou (CAbs(x, eval_cl_no [] b), c) *)
(*   | CApp (t1, t2), cont -> *)
(*     apply cont (eval_cl_bn (Clou(t1,[]))) t2 *)
(*   | Clou(_, _), _ -> t *)
(*   and apply ctxt f v = *)
(*     match f with *)
(*     | Clou (CAbs (x, b), c) -> eval_cl_no ((x, v)::c) b *)
(*     | _ -> CApp (eval_cl_no ctxt f, eval_cl_no ctxt v) *)

let rec no ct =
  match ct with
    | Clou (t, ctx) -> (
    match t, ctx with
      | CVar x, [] -> ct
      | CVar x as var , (y, n)::c ->
        if x = y then no n
        else no (Clou (var, c))
      | CAbs (x, b), c ->
        Clou (CAbs (x, no (Clou (b, clean_ctxt x c))), [])
      | CApp (m, n), c ->
        apply (bn (Clou(m,c))) (Clou (n, c))
      | _ -> failwith @@ "[Error] Evaluation of NormalOrder with Clousure. Term: "
        ^ string_of_cterm ct
    )
    | _ -> failwith @@ "[Error] Incorrect input in Clousure NormalOrder evaluation: "
        ^ string_of_cterm ct
and apply m n =
  match m with
    | Clou (CAbs(x, b), c) -> no (Clou(b, (x, n)::c))
    | _ -> Clou(CApp(no m, no n), [])
