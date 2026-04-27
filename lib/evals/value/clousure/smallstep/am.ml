open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open Core.Forms
open Bv
open Printer

let rec step_am = function
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, n)::env) ->
    if x = y then n
    else step_am (Clou(CVar x, env))
  | CAbs(x, b) when not (is_cvhnf b) ->
    let b' = step_am b in
    CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = step_am @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) when not (is_cwnf m) ->
    let m' = step_bv m in
    CApp(m', n)
  | CApp(v, n) when not (is_cwnf n) ->
    let n' = step_bv n in
    CApp(v, n')
  | CApp (CAbs(x, b), n) when is_cvalue n ->
    Clou(b, [(x, n)])
  | CApp (Clou(CAbs(x, b), env), n) when is_cvalue n ->
    Clou(b, (x, n)::env)
  | Clou(CApp(m, n), env) ->
    step_am (CApp(Clou(m, env), Clou(n, env)))
  | Clou(Clou(t, env1), env2) ->
    step_am (Clou(t, env1 @ env2))
  | _ -> failwith "not redex!"

let rec string_of_term_ss_am = string_of_cterm

let rec am t =
  if is_cvhnf t then t
  else (
    print_endline @@ string_of_term_ss_am t;
    t |> step_am |> am
  )
