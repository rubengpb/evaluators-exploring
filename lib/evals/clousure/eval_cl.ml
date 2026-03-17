open Core.Syntax
open Eval
open Core.Utils

let rec clean_ctxt x ctxt =
  List.filter (fun (k, _) -> k <> x) ctxt

let rec eval_cl_bv ctxt t =
  match t, ctxt with
  | CVar x, [] -> CVar x
  | CVar x, (y, v') :: c ->
    if x = y then v'
    else eval_cl_bv c (CVar x)
  | CAbs (x, b), c ->
    Clou (CAbs(x, b), c)
  | CApp (t1, t2), cont ->
    apply (eval_cl_bv cont t1) (eval_cl_bv cont t2)
  | Clou (t , c1), c2 -> eval_cl_bv (c1 @ c2) t
  and apply f v =
    match f with
    | Clou (CAbs (x, b), c) -> eval_cl_bv ((x, v)::c) b
    | _ -> CApp (f, v)

(* let rec eval_cl_bv ctxt t = *)
(*   match t, ctxt with *)
(*   | CVar x, Nihil -> CVar x *)
(*   | CVar x, Bind (y, v', c) -> *)
(*     if x = y then v' *)
(*     else eval_cl_bv c (CVar x) *)
(*   | CAbs (x, b), c -> *)
(*     Clou (CAbs(x, b), c) *)
(*   | CApp (t1, t2), cont -> *)
(*     apply (eval_cl_bv cont t1) (eval_cl_bv cont t2) *)
(*   | Clou (t , c1), c2 -> eval_cl_bv (append_ctxt c1 c2) t *)
(*   and apply f v = *)
(*     match f with *)
(*     | Clou (CAbs (x, b), c) -> eval_cl_bv (Bind (x, v, c)) b *)
(*     | _ -> CApp (f, v) *)

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
    apply_context c (subst (pure_of_clousure (eval_cl_bv [] v)) x t)



let rec clousure_of_pure = function
    | Var x -> CVar x
    | Abs (x, b) -> CAbs (x, clousure_of_pure b)
    | App (t1, t2) -> CApp (clousure_of_pure t1, clousure_of_pure t2)

let eval_cl_pure str t =
  let eval =
    match str with
      | CallByValue -> eval_cl_bv
      | CallByName -> eval_cl_bv
      | NormalOrder -> eval_cl_bv
      | ApplicativeOrder -> eval_cl_bv
      | _ -> eval_cl_bv
  in
  t |> clousure_of_pure |> (eval []) |> pure_of_clousure
