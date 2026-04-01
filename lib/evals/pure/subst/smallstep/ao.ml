open Core.Syntax
open Core.Utils
open Core.Forms
open Printer

let rec step_ao = function
  | Abs (x, body) -> Abs(x, step_ao body)
  | App (Var x, arg) -> App (Var x, step_ao arg)
  | App (Abs (x, body), arg) ->
    let is_body_nf = is_nf body in
    let is_arg_nf = is_nf arg in
    if  is_body_nf && is_arg_nf then subst arg x body
    else if is_body_nf then App(Abs(x, body), step_ao arg)
    else App(Abs(x, step_ao body), arg)
  | App (t1, t2) -> App(step_ao t1, t2)
  | t -> t (* Variables *)

let rec string_of_term_ss_ao = function
  | Var x -> x
  | Abs (x, t) ->
      "\\" ^ x ^ "." ^ string_of_term_ss_ao t
  | App (Var x1, Var x2) -> x1 ^ " " ^ x2
  | App (Var x, t) -> x ^ " (" ^ string_of_term_ss_ao t  ^ ")"
  | App (App(t1, t2), Var x) -> string_of_term_ss_ao (App (t1, t2))  ^ " " ^ x
  | App (App(t1, t2), Abs(x,t)) -> string_of_term_ss_ao (App (t1, t2))  ^ " (" ^ string_of_term_ss_ao (Abs(x,t))  ^ ")"
  | App (App(t1, t2), t) -> string_of_term_ss_ao (App (t1, t2)) ^ " (" ^ string_of_term_ss_ao t ^ ")"
  | App (Abs (x, body) as l, Var y) ->
      if is_there_redex body then
        "(" ^ string_of_term_ss_ao l ^ ")" ^ y
      else
        let left = colorize ("(" ^ string_of_pterm l ^ ")") "blue" in
        let right = colorize y "red" in
        left ^ " " ^ right
  | App (Abs (x, body) as l, arg) ->
      if is_there_redex body then
        "(" ^ string_of_term_ss_ao l ^ ") (" ^ string_of_term_ss_ao arg ^ ")"
      else
        let left = colorize ("(" ^ string_of_pterm l ^ ")") "blue" in
        let right = colorize ("(" ^ string_of_pterm arg ^ ")") "red" in
        left ^ " " ^ right

let rec ao t =
  if is_nf t then t
  else (
    print_endline @@ string_of_term_ss_ao t;
    t |> step_ao |> ao
  )
