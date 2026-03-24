open Core.Syntax
open Core.Utils
open Core.Forms
open Printer


let rec step_hn = function
  | Abs (x, body) -> Abs(x, step_hn body)
  | App (Abs (x, body), arg) -> subst arg x body
  | App (Var x, arg) -> App (Var x, step_hn arg)
  | App (t1, t2) -> App(step_hn t1, t2)
  | t -> t (* Variables *)

let rec string_of_term_ss_hn = function
  | Var x -> x
  | Abs (x, t) ->
      "\\" ^ x ^ "." ^ string_of_term_ss_hn t 
  | App (Var x1, Var x2) -> x1 ^ " " ^ x2
  | App (Var x, t) -> x ^ " (" ^ string_of_term_ss_hn t  ^ ")"
  | App (App(t1, t2), Var x) -> string_of_term_ss_hn (App (t1, t2))  ^ " " ^ x
  | App (App(t1, t2), Abs(x,t)) -> string_of_term_ss_hn (App (t1, t2))  ^ " (" ^ string_of_term_ss_hn (Abs(x,t))  ^ ")"
  | App (App(t1, t2), t) -> string_of_term_ss_hn (App (t1, t2)) ^ " (" ^ string_of_term_ss_hn t ^ ")"
  | App (Abs (x, body) as l, Var y) ->
      let left = colorize ("(" ^ string_of_pterm l ^ ")") "blue" in
      let right = colorize y "red" in
      left ^ " " ^ right
  | App (Abs (x, body) as l, arg) ->
      let left = colorize ("(" ^ string_of_pterm l ^ ")") "blue" in
      let right = colorize ("(" ^ string_of_pterm arg ^ ")") "red" in
      left ^ " " ^ right

let rec hn t =
  if is_nf t then t
  else (
    print_endline @@ string_of_term_ss_hn t;
    t |> step_hn |> hn
  )
