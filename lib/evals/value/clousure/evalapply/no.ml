open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open Value_utils
open Bn

let rec no = function
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, n)::env) -> if x = y then no n else no @@ Clou(CVar x, env)
  | CAbs(x, b) -> let b' = no b in CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = no @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) ->
    let m' = bn m in (
      match m' with
        | CAbs(x, b) ->
          if is_cvalue n then no @@ Clou(b, [(x, n)])
          else CApp(m', n)
        | Clou(CAbs(x, b), env) ->
          if is_cvalue n then no @@ Clou(b, (x, n)::env)
          else CApp(m', n)
        | _ -> let m'' = no m' in
          let n' = no n in
            CApp(m'', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = bn @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) ->
          if is_cvalue n then no @@ Clou(b, [(x, Clou(n, env))])
          else CApp(m', n)
        | Clou(CAbs(x, b), env') ->
          if is_cvalue n then no @@ Clou(b, (x, Clou(n, env))::env')
          else CApp(m', n)
        | _ -> let m'' = no m' in
          let n' = no @@ Clou(n, env) in
            CApp(m'', n')
    )
  | Clou(Clou(t, env1), env2) -> no @@ Clou(t, env1 @ env2)
