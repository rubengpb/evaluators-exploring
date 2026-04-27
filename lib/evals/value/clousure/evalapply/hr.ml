open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open Value_utils
open Bn

let rec hr = function
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, n)::env) -> if x = y then hr n else hr @@ Clou(CVar x, env)
  | CAbs(x, b) -> let b' = hr b in CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = hr @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) ->
    let m' = bn m in (
      match m' with
        | CAbs(x, b) ->
          if is_cvalue n then hr @@ Clou(b, [(x, n)])
          else CApp(m', n)
        | Clou(CAbs(x, b), env) ->
          if is_cvalue n then hr @@ Clou(b, (x, n)::env)
          else CApp(m', n)
        | _ -> let m'' = hr m' in
          CApp(m'', n)
    )
  | Clou(CApp(m, n), env) ->
    let m' = bn @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) ->
          if is_cvalue n then hr @@ Clou(b, [(x, Clou(n, env))])
          else CApp(m', n)
        | Clou(CAbs(x, b), env') ->
          if is_cvalue n then hr @@ Clou(b, (x, Clou(n, env))::env')
          else CApp(m', n)
        | _ -> let m'' = hr m' in CApp(m'', Clou(n, env))
    )
  | Clou(Clou(t, env1), env2) -> hr @@ Clou(t, env1 @ env2)
