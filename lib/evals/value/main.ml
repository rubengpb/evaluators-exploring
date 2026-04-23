open Core.Syntax
open Eval
open Subst.Main
open Debruijn.Main
open Clousure.Main

let eval_value e t =
  match e.subst with
    | Subst -> TPure (eval_subst e t)
    | DeBruijn -> TDeBruijn (eval_debruijn e t)
    | Clousure -> TClousure (eval_clousure e t)
