open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open Bn


let rec step_no = function
  | App(m, n) when not (is_whnf m) ->
    let m' = step_bn m in
    App(m', n)
  | App(Abs(x, b), n) when is_value n ->
    subst n x b
  | App(m, n) when not (is_nf m) ->
    let m' = step_no m in
    App(m', n)
  | App(v, n) when not (is_nf n) ->
    let n' = step_no n in
    App(v, n')
  | Abs(x, b) when not (is_nf b) ->
    let b' = step_no b in
    Abs(x, b')
  | _ -> failwith "Not redex!"

let rec string_of_term_ss_no = function
  | Var x -> x
  | Abs (x, t) ->
      "\\" ^ x ^ "." ^ string_of_term_ss_no t 
  | App (Var x1, Var x2) -> x1 ^ " " ^ x2
  | App (Var x, t) -> x ^ " (" ^ string_of_term_ss_no t  ^ ")"
  | App (App(t1, t2), Var x) -> string_of_term_ss_no (App (t1, t2))  ^ " " ^ x
  | App (App(t1, t2), Abs(x,t)) -> string_of_term_ss_no (App (t1, t2))  ^ " (" ^ string_of_term_ss_no (Abs(x,t))  ^ ")"
  | App (App(t1, t2), t) -> string_of_term_ss_no (App (t1, t2)) ^ " (" ^ string_of_term_ss_no t ^ ")"
  | App (Abs (x, body) as l, Var y) ->
      let left = colorize ("(" ^ string_of_pterm l ^ ")") "blue" in
      let right = colorize y "red" in
      left ^ " " ^ right
  | App (Abs (x, body) as l, arg) ->
      let left = colorize ("(" ^ string_of_pterm l ^ ")") "blue" in
      let right = colorize ("(" ^ string_of_pterm arg ^ ")") "red" in
      left ^ " " ^ right

let rec no t =
  if is_nf t then t
  else (
    print_endline @@ string_of_term_ss_no t;
    t |> step_no |> no
  )
