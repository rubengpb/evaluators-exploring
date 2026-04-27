open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open Value_utils
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
        | CAbs(x, b) ->
          let n' = bv n in
          if is_cvalue n' then sn @@ Clou(b, [(x, n')])
          else CApp(m', n')
        | Clou(CAbs(x, b), env) ->
          let n' = bv n in
          if is_cvalue n' then sn @@ Clou(b, (x, n')::env)
          else CApp(m', n')
        | _ ->
          let m'' = sn m' in
          let n' = sn n in
            CApp(m'', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = bv @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) ->
          let n' = bv @@ Clou(n, env) in
          if is_cvalue n' then sn @@ Clou(b, [(x, n')])
          else CApp(m', n')
        | Clou(CAbs(x, b), env') ->
          let n' = bv @@ Clou(n, env) in
          if is_cvalue n' then sn @@ Clou(b, (x, n')::env')
          else CApp(m', n')
        | _ ->
          let m'' = sn m' in
          let n' = sn @@ Clou(n, env) in
            CApp(m'', n')
    )
  | Clou(Clou(t, env1), env2) -> sn @@ Clou(t, env1 @ env2)
