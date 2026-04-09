open Eval
open Evalapply.Main
open Evalapplyzipper.Main
open Smallstep.Main
open Readback.Main
open Core.Utils

(* let eval_clou e t = *)
(*   match e with *)
(*     | Gen _ -> failwith "TODO" *)
(*     | One eval -> ( *)
(*       match eval.style, eval.strategy with *)
(*         | EvalApply, CallByValue -> TClousure (Cl_bv.eval_cl_bv [] t) *)
(*         | EvalApply, CallByName -> TClousure (Cl_bn.eval_cl_bn (Clou (t,[]))) *)
(*         | EvalApply, ApplicativeOrder -> TClousure (Cl_ao.eval_cl_ao (Clou (t, []))) *)
(*         | EvalApply, NormalOrder -> TClousure (Cl_no.eval_cl_no (Clou (t, []))) *)
(*         | _ -> failwith "TODO" *)
(*     ) *)

let eval_clousure e t =
  let t = clousure_of_pure t in
  match e.style with
    | EvalApply -> eval_apply e.strategy t
    | EvalApplyZipper -> eval_apply_zipper e.strategy t
    | SmallStep -> eval_smallstep e.strategy t
    | ReadBack -> eval_readback e.strategy t
