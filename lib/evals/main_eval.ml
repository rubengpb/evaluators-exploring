open Core.Strategy
open Nor
open Bv
open Ao

let eval strat t =
  match strat with
  | Normal -> eval_nor t
  | CallByValue -> eval_bv t
  | ApplicativeOrder -> eval_ao t
