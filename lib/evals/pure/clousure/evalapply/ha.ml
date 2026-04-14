open Core.Syntax
open Core.Utils
open Cl_utils
open Bv

let rec ha = function
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, v)::env) -> if x = y then v else ha @@ Clou(CVar x, env)
  | CAbs(x, b) -> let b' = ha b in CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = ha @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) ->
    let m' = bv m in
    let n' = ha n in (
      match m' with
        | CAbs(x, b) -> ha @@ Clou(b, [(x, n')])
        | Clou(CAbs(x, b), env) -> ha @@ Clou(b, (x, n')::env)
        | _ -> let m'' = ha m' in CApp(m'', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = bv @@ Clou(m, env) in
    let n' = ha @@ Clou(n, env) in (
      match m' with
        | CAbs(x, b) -> ha @@ Clou(b, [(x, Clou(n', env))])
        | Clou(CAbs(x, b), env') -> ha @@ Clou(b, (x, Clou(n', env))::env')
        | _ -> let m'' = m' in CApp(m'', n')
    )
  | Clou(Clou(t, env1), env2) -> ha @@ Clou(t, env1 @ env2)
