open Core.Syntax
open Core.Utils
open Cl_utils
open Core.Forms
open Bn
open Printer

let rec step_no = function
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, n)::env) ->
    if x = y then n
    else step_no (Clou(CVar x, env))
  | CAbs(x, b) when not (is_cnf b) ->
    let b' = step_no b in
    CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = step_no @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) when not (is_cwhnf m) ->
    let m' = step_bn m in
    CApp(m', n)
  | CApp (CAbs(x, b), n) ->
    Clou(b, [(x, n)])
  | CApp (Clou(CAbs(x, b), env), n) ->
    Clou(b, (x, n)::env)
  | CApp(m, n) when not (is_cnf m) ->
    let m' = step_no m in
    CApp(m', n)
  | CApp(v, n) when not (is_cnf n) ->
    let n' = step_no n in
    CApp(v, n')
  | Clou(CApp(m, n), env) ->
    step_no (CApp(Clou(m, env), Clou(n, env)))
  | Clou(Clou(t, env1), env2) ->
    step_no (Clou(t, env1 @ env2))
  | _ -> failwith "Not redex!"

let rec string_of_term_ss_no = string_of_cterm

let rec no t =
  if is_cnf t then t
  else (
    print_endline @@ string_of_term_ss_no t;
    t |> step_no |> no
  )
