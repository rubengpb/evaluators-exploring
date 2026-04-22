open Core.Syntax
open Core.Utils
open Cl_utils
open Core.Forms
open Bv
open Printer

let rec step_ha = function
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, n)::env) ->
    if x = y then n
    else step_ha (Clou(CVar x, env))
  | CAbs(x, b) when not (is_cnf b) ->
    let b' = step_ha b in
    CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = step_ha @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) when not (is_cwnf m) ->
    let m' = step_bv m in
    CApp(m', n)
  | CApp(v, n) when not (is_cnf n) ->
    let n' = step_ha n in
    CApp(v, n')
  | CApp (CAbs(x, b), n) ->
    Clou(b, [(x, n)])
  | CApp (Clou(CAbs(x, b), env), n) ->
    Clou(b, (x, n)::env)
  | CApp(m, n) when not (is_cnf m) ->
    let m' = step_ha m in
    CApp(m', n)
  | CApp(v, n) when not (is_cnf n) ->
    let n' = step_ha n in
    CApp(v, n')
  | Clou(CApp(m, n), env) ->
    step_ha (CApp(Clou(m, env), Clou(n, env)))
  | Clou(Clou(t, env1), env2) ->
    step_ha (Clou(t, env1 @ env2))
  | _ -> failwith "not redex!"

let rec string_of_term_ss_ha = string_of_cterm

let rec ha t =
  if is_cnf t then t
  else (
    print_endline @@ string_of_term_ss_ha t;
    t |> step_ha |> ha
  )
