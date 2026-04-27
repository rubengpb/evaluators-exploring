open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open Value_utils

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
    let m' = ao m in (
      match m' with
        | CAbs(x, b) ->
          let n' = ao n in
          if is_cvalue n' then ao @@ Clou(b, [(x, n')])
          else CApp(m', n')
        | Clou(CAbs(x, b), env) ->
          let n' = ao n in
          if is_cvalue n' then ao @@ Clou(b, (x, n')::env)
          else CApp(m', n')
        | _ ->
          let n' = ao n in
          CApp(m', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = ao @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) ->
          let n' = ao @@ Clou(n, env) in
          if is_cvalue n' then ao @@ Clou(b, [(x, n')])
          else CApp(m', n')
        | Clou(CAbs(x, b), env') ->
          let n' = ao @@ Clou(n, env) in
          if is_cvalue n' then ao @@ Clou(b, (x, n')::env')
          else CApp(m', n')
        | _ ->
          let n' = ao @@ Clou(n, env) in
          CApp(m', n')
    )
  | Clou(Clou(t, env1), env2) -> ao @@ Clou(t, env1 @ env2)
