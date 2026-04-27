open Core.Syntax
open Core.Utils
open Value_utils
open Pure.Clousure.Cl_utils

let rec he = function
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, v)::env) -> if x = y then v else he @@ Clou(CVar x, env)
  | CAbs(x, b) -> let b' = he b in CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = he @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) ->
    let m' = he m in (
      match m' with
        | CAbs(x, b) ->
          if is_cvalue n then he @@ Clou(b, [(x, n)])
          else CApp(m', n)
        | Clou(CAbs(x, b), env) ->
          if is_cvalue n then he @@ Clou(b, (x, n)::env)
          else CApp(m', n)
        | _ -> CApp(m', n)
    )
  | Clou(CApp(m, n), env) ->
    let m' = he @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) ->
          if is_cvalue n then he @@ Clou(b, [(x, Clou(n, env))])
          else CApp(m', n)
        | Clou(CAbs(x, b), env') ->
          if is_cvalue n then he @@ Clou(b, (x, Clou(n, env))::env')
          else CApp(m', n)
        | _ -> CApp(m', Clou(n, env))
    )
  | Clou(Clou(t, env1), env2) -> he @@ Clou(t, env1 @ env2)
