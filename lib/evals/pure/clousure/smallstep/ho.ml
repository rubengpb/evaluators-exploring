open Core.Syntax
open Core.Utils
open Cl_utils
open Core.Forms
open Printer

let rec step_ho = function
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, n)::env) ->
    if x = y then n
    else step_ho (Clou(CVar x, env))
  | CAbs(x, b) when not (is_chnf b) ->
    let b' = step_ho b in
    CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = step_ho @@ Clou(b, (x, CVar var)::env) in
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
  | Clou(CApp(m, n), env) ->
    step_ho (CApp(Clou(m, env), Clou(n, env)))
  | Clou(Clou(t, env1), env2) ->
    step_ho (Clou(t, env1 @ env2))
  | _ -> failwith "not redex!"

let rec string_of_term_ss_ho = string_of_cterm

let rec ho t =
  if is_chnf t then t
  else (
    print_endline @@ string_of_term_ss_ho t;
    t |> step_ho |> ho
  )
