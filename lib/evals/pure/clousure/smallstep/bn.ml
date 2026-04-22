open Core.Syntax
open Core.Utils
open Core.Forms
open Printer

let rec step_bn = function
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, n)::env) ->
    if x = y then n
    else step_bn (Clou(CVar x, env))
  | CApp(m, n) when not (is_cwhnf m) ->
    let m' = step_bn m in
    CApp(m', n)
  | CApp (CAbs(x, b), n) ->
    Clou(b, [(x, n)])
  | CApp (Clou(CAbs(x, b), env), n) ->
    Clou(b, (x, n)::env)
  | Clou(CApp(m, n), env) ->
    step_bn (CApp(Clou(m, env), Clou(n, env)))
  | Clou(Clou(t, env1), env2) ->
    step_bn (Clou(t, env1 @ env2))
  | _ -> failwith "Not redex!"

let rec string_of_term_ss_bn = string_of_cterm

let rec bn t =
  if is_cwhnf t then t
  else (
    print_endline @@ string_of_term_ss_bn t;
    t |> step_bn |> bn
  )
