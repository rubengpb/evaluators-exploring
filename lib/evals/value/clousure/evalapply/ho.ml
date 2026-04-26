open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils

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
        | CAbs(x, b) -> ho @@ Clou(b, [(x, ho n)])
        | Clou(CAbs(x, b), env) -> ho @@ Clou(b, (x, ho n)::env)
        | _ -> CApp(m', n)
    )
  | Clou(CApp(m, n), env) ->
    let m' = ho @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) -> ho @@ Clou(b, [(x, ho @@ Clou(n, env))])
        | Clou(CAbs(x, b), env') -> ho @@ Clou(b, (x, ho @@ Clou(n, env))::env')
        | _ -> CApp(m', Clou(n, env))
    )
  | Clou(Clou(t, env1), env2) -> ho @@ Clou(t, env1 @ env2)
