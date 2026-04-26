open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils
open He

let rec hn = function
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, v)::env) -> if x = y then v else hn @@ Clou(CVar x, env)
  | CAbs(x, b) -> let b' = hn b in CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = hn @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) ->
    let m' = he m in (
      match m' with
        | CAbs(x, b) -> hn @@ Clou(b, [(x, n)])
        | Clou(CAbs(x, b), env) -> hn @@ Clou(b, (x, n)::env)
        | _ ->
          let m'' = hn m' in
          let n' = hn n in
            CApp(m'', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = he @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) -> hn @@ Clou(b, [(x, Clou(n, env))])
        | Clou(CAbs(x, b), env') -> hn @@ Clou(b, (x, Clou(n, env))::env')
        | _ ->
          let m'' = hn m' in
          let n' = hn n in
            CApp(m'', n')
    )
  | Clou(Clou(t, env1), env2) -> hn @@ Clou(t, env1 @ env2)
