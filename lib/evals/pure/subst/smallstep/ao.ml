open Core.Syntax
open Core.Utils
open Core.Forms
open Printer

let rec step_ao = function
  | App(m, n) when not (is_nf m) ->
    let m' = step_ao m in
    App(m', n)
  | App(v, n) when not (is_nf n) ->
    let n' = step_ao n in
    App(v, n')
  | App(Abs(x, b), v) ->
    subst v x b
  | Abs(x, b) when not (is_nf b)->
    let b' = step_ao b in
    Abs(x, b')
  | _ -> failwith "Not redex!"

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
