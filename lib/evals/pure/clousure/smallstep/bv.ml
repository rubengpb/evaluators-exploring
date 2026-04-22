open Core.Syntax
open Core.Utils
open Core.Forms
open Printer

let rec step_bv = function
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, v)::env) ->
    if x = y then v
    else step_bv (Clou(CVar x, env))
  | CApp(m, n) when not (is_cwnf m) ->
    let m' = step_bv m in
    CApp(m', n)
  | CApp(v, n) when not (is_cwnf n) ->
    let n' = step_bv n in
    CApp(v, n')
  | CApp (CAbs(x, b), v) ->
    Clou(b, [(x, v)])
  | CApp (Clou(CAbs(x, b), env), v) ->
    Clou(b, (x, v)::env)
  | Clou(CApp(m, n), env) ->
    step_bv (CApp(Clou(m, env), Clou(n, env)))
  | _ -> failwith "Not redex!"

let rec string_of_term_ss_bv = string_of_cterm

let rec bv t =
  if is_cwnf t then t
  else (
    print_endline @@ string_of_term_ss_bv t;
    t |> step_bv |> bv
  )
