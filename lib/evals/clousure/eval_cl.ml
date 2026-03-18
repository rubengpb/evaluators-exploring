open Core.Syntax
open Eval
open Core.Utils

let rec clean_ctxt x ctxt =
  List.filter (fun (k, _) -> k <> x) ctxt

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
    apply_context c (subst (pure_of_clousure (Cl_bv.eval_cl_bv [] v)) x t)

(* let eval_cl_pure str t = *)
(*   let eval = *)
(*     match str with *)
(*       | CallByValue -> eval_cl_bv *)
(*       | CallByName -> eval_cl_bv *)
(*       | NormalOrder -> eval_cl_bv *)
(*       | ApplicativeOrder -> eval_cl_bv *)
(*       | _ -> eval_cl_bv *)
(*   in *)
(*   t |> clousure_of_pure |> (eval []) |> pure_of_clousure *)
