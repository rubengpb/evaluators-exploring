open Eval
open Core.Utils
open Core.Forms
open Pure.Main
open Value.Main

let eval e t =
  match e.language with
    | Pure -> eval_pure e t
    | Value -> eval_value e t
