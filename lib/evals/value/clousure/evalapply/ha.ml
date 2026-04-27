open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open Value_utils
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
    let m' = bv m in (
      match m' with
        | CAbs(x, b) ->
          let n' = ha n in
          if is_cvalue n' then ha @@ Clou(b, [(x, n')])
          else CApp(m', n')
        | Clou(CAbs(x, b), env) ->
          let n' = ha n in
          if is_cvalue n' then ha @@ Clou(b, (x, n')::env)
          else CApp(m', n')
        | _ -> let m'' = ha m' in
          let n' = ha n in
          CApp(m'', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = bv @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) ->
          let n' = ha @@ Clou(n, env) in
          if is_cvalue n' then ha @@ Clou(b, [(x, Clou(n', env))])
          else CApp(m', n')
        | Clou(CAbs(x, b), env') ->
          let n' = ha @@ Clou(n, env) in
          if is_cvalue n' then ha @@ Clou(b, (x, Clou(n', env))::env')
          else CApp(m', n')
        | _ -> let m'' = m' in
          let n' = ha @@ Clou(n, env) in
          CApp(m'', n')
    )
  | Clou(Clou(t, env1), env2) -> ha @@ Clou(t, env1 @ env2)
