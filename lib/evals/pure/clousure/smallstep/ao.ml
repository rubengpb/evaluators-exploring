open Core.Syntax
open Core.Utils
open Cl_utils
open Core.Forms
open Printer

let rec step_ao = function
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, v)::env) ->
    if x = y then v
    else step_ao (Clou(CVar x, env))
  | CAbs(x, b) when not (is_cnf b) ->
    let b' = step_ao b in
    CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = step_ao @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) when not (is_cnf m) ->
    let m' = step_ao m in
    CApp(m', n)
  | CApp(v, n) when not (is_cnf n) ->
    let n' = step_ao n in
    CApp(v, n')
  | CApp (CAbs(x, b), v) ->
    Clou(b, [(x, v)])
  | CApp (Clou(CAbs(x, b), env), v) ->
    Clou(b, (x, v)::env)
  | Clou(CApp(m, n), env) ->
    step_ao (CApp(Clou(m, env), Clou(n, env)))
  | Clou(Clou(t, env1), env2) ->
    step_ao (Clou(t, env1 @ env2))
  | _ -> failwith "Not redex!"

let rec string_of_term_ss_ao = string_of_cterm

let rec ao t =
  if is_cnf t then t
  else (
    print_endline @@ string_of_term_ss_ao t;
    t |> step_ao |> ao
  )
