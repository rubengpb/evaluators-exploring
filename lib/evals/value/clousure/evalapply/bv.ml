open Core.Syntax
open Value_utils

let rec bv = function
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, v)::env) -> if x = y then v else bv @@ Clou(CVar x, env)
  | CAbs(_,_) as abs -> abs
  | Clou(CAbs(_,_),_) as abs -> abs
  | CApp(m, n) ->
    let m' = bv m in
    let n' = bv n in (
      match m' with
        | CAbs(x, b) ->
          if is_cvalue n' then bv @@ Clou(b, [(x, n')])
          else CApp(m', n')
        | Clou(CAbs(x, b), env) ->
          if is_cvalue n' then bv @@ Clou(b, (x, n')::env)
          else CApp(m', n')
        | _ -> CApp(m', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = bv @@ Clou(m, env) in
    let n' = bv @@ Clou(n, env) in (
      match m' with
        | CAbs(x, b) ->
          if is_cvalue n' then bv @@ Clou(b, [(x, n')])
          else CApp(m', n')
        | Clou(CAbs(x, b), env) ->
          if is_cvalue n' then bv @@ Clou(b, (x, n')::env)
          else CApp(m', n')
        | _ -> CApp(m', n')
    )
  | Clou(Clou(t, env1), env2) -> bv @@ Clou(t, env1 @ env2)
