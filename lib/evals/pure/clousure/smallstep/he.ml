open Core.Syntax
open Core.Utils
open Cl_utils
open Core.Forms
open Printer

let rec step_he = function
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, n)::env) ->
    if x = y then n
    else step_he (Clou(CVar x, env))
  | CAbs(x, b) when not (is_chnf b) ->
    let b' = step_he b in
    CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = step_he @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) when not (is_chnf m) ->
    let m' = step_he m in
    CApp(m', n)
  | CApp (CAbs(x, b), n) ->
    Clou(b, [(x, n)])
  | CApp (Clou(CAbs(x, b), env), n) ->
    Clou(b, (x, n)::env)
  | Clou(CApp(m, n), env) ->
    step_he (CApp(Clou(m, env), Clou(n, env)))
  | _ -> failwith "not redex!"

let rec string_of_term_ss_he = string_of_cterm

let rec he t =
  if is_chnf t then t
  else (
    print_endline @@ string_of_term_ss_he t;
    t |> step_he |> he
  )
