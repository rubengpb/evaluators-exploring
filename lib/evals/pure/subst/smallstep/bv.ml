open Core.Syntax
open Core.Utils
open Core.Forms
open Printer


let rec string_of_term_ss_bv = function
  | Var x -> x
  | Abs (x, t) ->
      "\\" ^ x ^ "." ^ string_of_term_ss_bv t 
  | App (Var x1, Var x2) -> x1 ^ " " ^ x2
  | App (Var x, t) -> x ^ " (" ^ string_of_term_ss_bv t  ^ ")"
  | App (App(t1, t2), Var x) -> string_of_term_ss_bv (App (t1, t2))  ^ " " ^ x
  | App (App(t1, t2), Abs(x,t)) -> string_of_term_ss_bv (App (t1, t2))  ^ " (" ^ string_of_term_ss_bv (Abs(x,t))  ^ ")"
  | App (App(t1, t2), t) -> string_of_term_ss_bv (App (t1, t2)) ^ " (" ^ string_of_term_ss_bv t ^ ")"
  | App (Abs (x, body) as l, Var y) ->
      let left = colorize ("(" ^ string_of_pterm l ^ ")") "blue" in
      let right = colorize y "red" in
      left ^ " " ^ right
  | App (Abs (x, body) as l, arg) ->
      let left = colorize ("(" ^ string_of_pterm l ^ ")") "blue" in
      let right = colorize ("(" ^ string_of_pterm arg ^ ")") "red" in
      left ^ " " ^ right

let rec step_bv = function
  | App (Abs (x, b), m) ->
      subst m x b
  | App(m, n) when not (is_value m) ->
    let m' = step_bv m in
    App(m', n)
  | App(v, n) when not (is_value n) ->
    let n' = step_bv n in
    App(v, n')
  | _ -> failwith "Not redex!"

let rec bv t =
  if is_wnf t then t
  else (
    print_endline @@ string_of_term_ss_bv t;
    t |> step_bv |> bv
  )
