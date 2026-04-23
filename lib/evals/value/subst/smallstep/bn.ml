open Core.Syntax
open Core.Utils
open Core.Forms
open Printer


let rec step_bn = function
  | App (Abs (x, b), n) when is_value n ->
      subst n x b
  | App (m, n) ->
      let m' = step_bn m in
      App (m', n)
  | _ -> failwith "Not redex!"

let rec string_of_term_ss_bn = function
  | Var x -> x
  | Abs (x, t) ->
      "\\" ^ x ^ "." ^ string_of_pterm t
  | App (Var x1, Var x2) -> x1 ^ " " ^ x2
  | App (Var x, t) -> x ^ " (" ^ string_of_term_ss_bn t  ^ ")"
  | App (App(t1, t2), Var x) ->
    string_of_term_ss_bn (App (t1, t2))  ^ " " ^ x
  | App (App(t1, t2), Abs(x,t)) ->
    string_of_term_ss_bn (App (t1, t2))  ^ " (" ^ string_of_pterm (Abs(x,t))  ^ ")"
  | App (App(t1, t2), t) ->
    string_of_term_ss_bn (App (t1, t2)) ^ " (" ^ string_of_pterm t ^ ")"
  | App (Abs (x, body) as l, Var y) ->
      let left = colorize ("(" ^ string_of_pterm l ^ ")") "blue" in
      let right = colorize y "red" in
      left ^ " " ^ right
  | App (Abs (x, body) as l, arg) ->
      let left = colorize ("(" ^ string_of_pterm l ^ ")") "blue" in
      let right = colorize ("(" ^ string_of_pterm arg ^ ")") "red" in
      left ^ " " ^ right

let rec bn t =
  if is_whnf t then t
  else (
    print_endline @@ string_of_term_ss_bn t;
    t |> step_bn |> bn
  )
