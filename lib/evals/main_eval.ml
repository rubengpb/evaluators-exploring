open Eval
open Bv
open Bn
open Ao
open Nor
open Hr
open He
open Sn
open Hn
open Ha
open Am
open Ho
open So
open Bs

let eval strat t =
  match strat with
  | CallByValue -> eval_bv t
  | CallByName -> eval_bn t
  | ApplicativeOrder -> eval_ao t
  | Normal -> eval_nor t
  | HeadReduction -> eval_hr t
  | HeadSpine -> eval_he t
  | StricNormalisation -> eval_sn t
  | HybridNormalOrder -> eval_hn t
  | HybridApplicativeOrder -> eval_ha t
  | AheadMachine -> eval_am t
  | HeadApplicativeOrder -> eval_ho t
  | SpineApplicativeOrder -> eval_so t
  | BalancedSpineApplicativeOrder -> eval_bs t
