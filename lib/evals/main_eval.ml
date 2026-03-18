open Eval
open Gen
open Core.Utils
open Core.Forms

let eval e t =
  match get_language e with
    | Pure -> Pure.eval_pure e t
    | Clousure -> Clousure.eval_clou e (clousure_of_pure t)
