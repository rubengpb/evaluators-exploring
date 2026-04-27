open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open Value_utils
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
    let m' = ho m in (
      match m' with
        | CAbs(x, b) ->
          let n' = so n in
          if is_cvalue n' then so @@ Clou(b, [(x, n')])
          else CApp(m', n')
        | Clou(CAbs(x, b), env) ->
          let n' = so n in
          if is_cvalue n' then so @@ Clou(b, (x, n')::env)
          else CApp(m', n')
        | _ -> let m'' = so m' in
          let n' = so n in
          CApp(m'', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = ho @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) ->
          let n' = so @@ Clou(n, env) in
          if is_cvalue n' then so @@ Clou(b, [(x, Clou(n', env))])
          else CApp(m', n')
        | Clou(CAbs(x, b), env') ->
          let n' = so @@ Clou(n, env) in
          if is_cvalue n' then so @@ Clou(b, (x, Clou(n', env))::env')
          else CApp(m', n')
        | _ -> let m'' = m' in
          let n' = so @@ Clou(n, env) in
          CApp(m'', n')
    )
  | Clou(Clou(t, env1), env2) -> so @@ Clou(t, env1 @ env2)
