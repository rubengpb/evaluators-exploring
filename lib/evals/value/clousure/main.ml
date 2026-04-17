open Eval
open Evalapply.Main
open Evalapplyzipper.Main
open Smallstep.Main
open Readback.Main

let eval_clousure e t =
  match e.style with
    | EvalApply -> eval_apply e.strategy t
    | EvalApplyZipper -> eval_apply_zipper e.strategy t
    | SmallStep -> eval_smallstep e.strategy t
    | ReadBack -> eval_readback e.strategy t
