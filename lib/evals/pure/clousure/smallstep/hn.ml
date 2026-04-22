open Core.Syntax
open Core.Utils
open Cl_utils
open Core.Forms
open He
open Printer

let rec step_hn = function
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, n)::env) ->
    if x = y then n
    else step_hn (Clou(CVar x, env))
  | CAbs(x, b) when not (is_cnf b) ->
    let b' = step_hn b in
    CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = step_hn @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) when not (is_chnf m) ->
    let m' = step_he m in
    CApp(m', n)
  | CApp (CAbs(x, b), n) ->
    Clou(b, [(x, n)])
  | CApp (Clou(CAbs(x, b), env), n) ->
    Clou(b, (x, n)::env)
  | CApp(m, n) when not (is_cnf m) ->
    let m' = step_hn m in
    CApp(m', n)
  | CApp(v, n) when not (is_cnf n) ->
    let n' = step_hn n in
    CApp(v, n')
  | Clou(CApp(m, n), env) ->
    step_hn (CApp(Clou(m, env), Clou(n, env)))
  | _ -> failwith "not redex!"

let rec string_of_term_ss_hn = string_of_cterm

let rec hn t =
  if is_cnf t then t
  else (
    print_endline @@ string_of_term_ss_hn t;
    t |> step_hn |> hn
  )
