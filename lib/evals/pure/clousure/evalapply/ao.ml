open Core.Syntax
open Core.Utils
open Cl_utils

let rec ao = function
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, v)::env) -> if x = y then v else ao @@ Clou(CVar x, env)
  | CAbs(x, b) -> let b' = ao b in CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = ao @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) ->
    let m' = ao m in
    let n' = ao n in (
      match m' with
        | CAbs(x, b) -> ao @@ Clou(b, [(x, n')])
        | Clou(CAbs(x, b), env) -> ao @@ Clou(b, (x, n')::env)
        | _ -> CApp(m', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = ao @@ Clou(m, env) in
    let n' = ao @@ Clou(n, env) in (
      match m' with
        | CAbs(x, b) -> ao @@ Clou(b, [(x, Clou(n', env))])
        | Clou(CAbs(x, b), env') -> ao @@ Clou(b, (x, Clou(n', env))::env')
        | _ -> CApp(m', n')
    )
  | Clou(Clou(t, env1), env2) -> ao @@ Clou(t, env1 @ env2)
