open Core.Syntax
open Cl_utils
open Core.Utils

(* let rec eval_cl_ao ctxt t = *)
(*   match t, ctxt with *)
(*   | CVar x, [] -> CVar x *)
(*   | CVar x, (y, v) :: c -> *)
(*     if x = y then v *)
(*     else eval_cl_ao c (CVar x) *)
(*   | CAbs (x, b), c -> *)
(*     Clou (CAbs(x, eval_cl_ao [] b), c) *)
(*   | CApp (t1, t2), cont -> *)
(*     apply (eval_cl_ao cont t1) (eval_cl_ao cont t2) *)
(*   | _ -> failwith  *)
(*   and apply m n = *)
(*     match m with *)
(*     | Clou (CAbs (x, b), c) -> eval_cl_ao ((x, v)::c) b *)
(*     | _ -> Clou(CApp (m, n), []) *)

let rec ao ct =
  match ct with
    | Clou (t, ctx) -> (
    match t, ctx with
      | CVar x, [] -> ct
      | CVar x as var , (y, v)::c ->
        if x = y then v
        else ao (Clou (var, c))
      | CAbs (x, b), c ->
        Clou (CAbs (x, ao (Clou (b, clean_ctxt x c))), [])
      | CApp (m, n), c ->
        apply (ao (Clou(m,c))) (ao (Clou (n, c)))
      | _ -> failwith @@ "[Error] Evaluation of ApplicativeOrder with Clousure. Term: "
        ^ string_of_cterm ct
    )
    | _ -> failwith @@ "[Error] Incorrect input in Clousure ApplicativeOrder evaluation: "
        ^ string_of_cterm ct
and apply m n =
  match m with
    | Clou (CAbs(x, b), c) -> ao (Clou(b, (x, n)::c))
    | _ -> Clou(CApp(m, n), [])
