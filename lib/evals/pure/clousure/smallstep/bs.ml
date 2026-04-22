open Core.Syntax
open Core.Utils
open Cl_utils
open Core.Forms
open Ho
open Printer

let rec step_bs = function
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, n)::env) ->
    if x = y then n
    else step_bs (Clou(CVar x, env))
  | CAbs(x, b) when not (is_cnf b) ->
    let b' = step_bs b in
    CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = step_bs @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) when not (is_chnf m) ->
    let m' = step_ho m in
    CApp(m', n)
  | CApp(v, n) when not (is_chnf n) ->
    let n' = step_ho n in
    CApp(v, n')
  | CApp (CAbs(x, b), n) ->
    Clou(b, [(x, n)])
  | CApp (Clou(CAbs(x, b), env), n) ->
    Clou(b, (x, n)::env)
  | CApp(m, n) when not (is_cnf m) ->
    let m' = step_bs m in
    CApp(m', n)
  | CApp(v, n) when not (is_cnf n) ->
    let n' = step_bs n in
    CApp(v, n')
  | Clou(CApp(m, n), env) ->
    step_bs (CApp(Clou(m, env), Clou(n, env)))
  | Clou(Clou(t, env1), env2) ->
    step_bs (Clou(t, env1 @ env2))
  | _ -> failwith "not redex!"

let rec string_of_term_ss_bs = string_of_cterm

let rec bs t =
  if is_cnf t then t
  else (
    print_endline @@ string_of_term_ss_bs t;
    t |> step_bs |> bs
  )
