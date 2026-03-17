open Core.Syntax
open Core.Utils

type instruction =
  | Quit
  | IConfig of string option
  | Set of string * string
  | Type of string
  | Help of string option
  | Load of string

(* type var = string *)
(**)
(* type term = *)
(*   | Var of var *)
(*   | Abs of var * term *)
(*   | App of term * term *)

type command =
  | Instr of instruction
  | Term of pterm
  | Assign of var * pterm

let show_term = string_of_pterm

let show_instruction = function
  | Quit -> ":q"
  | IConfig i ->
    (match i with
      | Some s -> ":config " ^ s
      | None -> ":config")
  | Set (param, opt) -> ":set " ^ param ^ " " ^ opt
  | Type x -> ":t " ^ x
  | Help h ->
    (match h with
      | Some s -> ":h " ^ s
      | None -> ":h")
  | Load x -> ":load" ^ x

let show_command = function
  | Instr i -> show_instruction i
  | Assign (v, t) -> v ^ " = " ^ show_term t
  | Term t -> show_term t
