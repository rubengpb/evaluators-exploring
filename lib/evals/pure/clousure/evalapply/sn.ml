open Core.Syntax
open Core.Utils
open Cl_utils
open Bv

let rec sn = function
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, v)::env) -> if x = y then v else sn @@ Clou(CVar x, env)
  | CAbs(x, b) -> let b' = sn b in CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = sn @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) ->
    let m' = bv m in (
      match m' with
        | CAbs(x, b) -> sn @@ Clou(b, [(x, bv n)])
        | Clou(CAbs(x, b), env) -> sn @@ Clou(b, (x, bv n)::env)
        | _ ->
          let m'' = sn m' in
          let n' = sn n in
            CApp(m'', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = bv @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) -> sn @@ Clou(b, [(x, bv @@ Clou(n, env))])
        | Clou(CAbs(x, b), env') -> sn @@ Clou(b, (x, bv @@ Clou(n, env))::env')
        | _ ->
          let m'' = sn m' in
          let n' = sn @@ Clou(n, env) in
            CApp(m'', n')
    )
  | Clou(Clou(t, env1), env2) -> sn @@ Clou(t, env1 @ env2)
