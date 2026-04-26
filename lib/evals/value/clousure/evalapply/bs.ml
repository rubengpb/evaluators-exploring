open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open Ho

let rec bs = function
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, v)::env) -> if x = y then v else bs @@ Clou(CVar x, env)
  | CAbs(x, b) -> let b' = bs b in CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = bs @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) ->
    let m' = ho m in
    let n' = ho n in (
      match m' with
        | CAbs(x, b) -> bs @@ Clou(b, [(x, n')])
        | Clou(CAbs(x, b), env) -> bs @@ Clou(b, (x, n')::env)
        | _ -> CApp(bs m', bs n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = ho @@ Clou(m, env) in
    let n' = ho @@ Clou(n, env) in (
      match m' with
        | CAbs(x, b) -> bs @@ Clou(b, [(x, Clou(n', env))])
        | Clou(CAbs(x, b), env') -> bs @@ Clou(b, (x, Clou(n', env))::env')
        | _ -> CApp(bs m', bs n')
    )
  | Clou(Clou(t, env1), env2) -> bs @@ Clou(t, env1 @ env2)
