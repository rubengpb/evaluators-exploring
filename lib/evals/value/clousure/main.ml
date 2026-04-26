open Eval
open Evalapply.Main
open Evalapplyzipper.Main
open Smallstep.Main
open Readback.Main
open Core.Utils
open Pure.Clousure.Cl_utils

let eval_clousure e t =
  cl_reset();
  let t = clousure_of_pure t in
  match e.style with
    | EvalApply -> eval_apply e.strategy t
    | EvalApplyZipper -> eval_apply_zipper e.strategy t
    | SmallStep -> eval_smallstep e.strategy t
    | ReadBack -> eval_readback e.strategy t
