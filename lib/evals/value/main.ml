open Core.Syntax
open Eval
open Subst.Main
open Clousure.Main

let eval_value e t =
  match e.subst with
    | Subst -> TPure (eval_subst e t)
    | DeBruijn -> failwith "TODO"
    | Clousure -> TClousure (eval_clousure e t)
