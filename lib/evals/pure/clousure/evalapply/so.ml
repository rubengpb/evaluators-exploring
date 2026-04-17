open Core.Syntax
open Core.Utils
open Cl_utils
open Ho

let rec so = function
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, v)::env) -> if x = y then v else so @@ Clou(CVar x, env)
  | CAbs(x, b) -> let b' = so b in CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = so @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) ->
    let m' = ho m in
    let n' = so n in (
      match m' with
        | CAbs(x, b) -> so @@ Clou(b, [(x, n')])
        | Clou(CAbs(x, b), env) -> so @@ Clou(b, (x, n')::env)
        | _ -> let m'' = so m' in CApp(m'', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = ho @@ Clou(m, env) in
    let n' = so @@ Clou(n, env) in (
      match m' with
        | CAbs(x, b) -> so @@ Clou(b, [(x, Clou(n', env))])
        | Clou(CAbs(x, b), env') -> so @@ Clou(b, (x, Clou(n', env))::env')
        | _ -> let m'' = m' in CApp(m'', n')
    )
  | Clou(Clou(t, env1), env2) -> so @@ Clou(t, env1 @ env2)
