open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open Value_utils

let rec ho = function
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, v)::env) -> if x = y then v else ho @@ Clou(CVar x, env)
  | CAbs(x, b) -> let b' = ho b in CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^  string_of_int (cl_fresh ()) in
    let b' = ho @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) ->
    let m' = ho m in (
      match m' with
        | CAbs(x, b) ->
          let n' = ho n in
          if is_cvalue n' then ho @@ Clou(b, [(x, n')])
          else CApp(m', n')
        | Clou(CAbs(x, b), env) ->
          let n' = ho n in
          if is_cvalue n' then ho @@ Clou(b, (x, n')::env)
          else CApp(m', n')
        | _ -> CApp(m', n)
    )
  | Clou(CApp(m, n), env) ->
    let m' = ho @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) ->
          let n' = ho @@ Clou(n, env) in
          if is_cvalue n' then ho @@ Clou(b, [(x, n')])
          else CApp(m', n')
        | Clou(CAbs(x, b), env') ->
          let n' = ho @@ Clou(n, env) in
          if is_cvalue n' then ho @@ Clou(b, (x, n')::env')
          else CApp(m', n')
        | _ -> CApp(m', Clou(n, env))
    )
  | Clou(Clou(t, env1), env2) -> ho @@ Clou(t, env1 @ env2)
