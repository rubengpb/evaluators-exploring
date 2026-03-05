open Core.Syntax
open Core.Subst

type instruction =
  | Quit
  | Info
  | Set of string
  | Type of string
  | Help
  | Envm

(* type var = string *)
(**)
(* type term = *)
(*   | Var of var *)
(*   | Abs of var * term *)
(*   | App of term * term *)

type command =
  | Instr of instruction
  | Term of term
  | Assign of var * term

let rec show_term = term_to_string

let show_instruction = function
  | Quit -> ":q"
  | Info -> ":info"
  | Set x -> ":set " ^ x
  | Type x -> ":t " ^ x
  | Help -> ":h"
  | Envm -> ":env"

let show_command = function
  | Instr i -> show_instruction i
  | Assign (v, t) -> v ^ " = " ^ show_term t
  | Term t -> show_term t
