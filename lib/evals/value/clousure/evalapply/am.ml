open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open Value_utils
open Bv

let rec am = function
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, v)::env) -> if x = y then v else am @@ Clou(CVar x, env)
  | CAbs(x, b) -> let b' = am b in CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = am @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) ->
    let m' = bv m in (
      match m' with
        | CAbs(x, b) ->
          let n' = bv n in
          if is_cvalue n' then am @@ Clou(b, [(x, n')])
          else CApp(m', n')
        | Clou(CAbs(x, b), env) ->
          let n' = bv n in
          if is_cvalue n' then am @@ Clou(b, (x, n')::env)
          else CApp(m', n')
        | _ ->
          let n' = bv n in
          let m'' = am m' in CApp(m'', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = bv @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) ->
          let n' = bv @@ Clou(n, env) in
          if is_cvalue n' then am @@ Clou(b, [(x, Clou(n', env))])
          else CApp(m', n')
        | Clou(CAbs(x, b), env') ->
          let n' = bv @@ Clou(n, env) in
          if is_cvalue n' then am @@ Clou(b, (x, Clou(n', env))::env')
          else CApp(m', n')
        | _ -> let m'' = am m' in
          let n' = bv @@ Clou(n, env) in
          CApp(m'', n')
    )
  | Clou(Clou(t, env1), env2) -> am @@ Clou(t, env1 @ env2)
