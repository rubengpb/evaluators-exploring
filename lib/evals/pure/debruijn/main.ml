open Eval
open Evalapply.Main
open Evalapplyzipper.Main
open Smallstep.Main
open Readback.Main
open Core.Utils

let eval_debruijn e t =
  let t = dbterm_of_pterm t in
  match e.style with
    | EvalApply -> eval_apply e.strategy t
    | EvalApplyZipper -> eval_apply_zipper e.strategy t
    | SmallStep -> eval_smallstep e.strategy t
    | ReadBack -> failwith "TODO"
    (* | ReadBack -> eval_readback e.strategy t *)
