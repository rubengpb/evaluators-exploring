open Core.Strategy
open Bv
open Bn
open Ao
open Nor
open Hr
open He

let eval strat t =
  match strat with
  | CallByValue -> eval_bv t
  | CallByName -> eval_bn t
  | ApplicativeOrder -> eval_ao t
  | Normal -> eval_nor t
  | HeadReduction -> eval_hr t
  | HeadSpine -> eval_he t
