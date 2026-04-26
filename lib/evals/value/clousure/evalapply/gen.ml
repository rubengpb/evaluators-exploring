open Core.Syntax
open Core.Utils
open Pure.Clousure.Cl_utils

let rec gen la op1 ar1 op2 ar2 t =
  let gen_aux = gen la op1 ar1 op2 ar2 in
  match t with
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, n)::env) -> if x = y then gen_aux n else gen_aux @@ Clou(CVar x, env)
  | CAbs(x, b) -> let b' = la b in CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = la @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) ->
    let m' = op1 m in (
      match m' with
        | CAbs(x, b) -> gen_aux @@ Clou(b, [(x, ar1 n)])
        | Clou(CAbs(x, b), env) -> gen_aux @@ Clou(b, (x, ar1 n)::env)
        | _ -> CApp(op2 m', ar2 n)
    )
  | Clou(CApp(m, n), env) ->
    let m' = op1 @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) -> gen_aux @@ Clou(b, [(x, ar1 @@ Clou(n, env))])
        | Clou(CAbs(x, b), env') -> gen_aux @@ Clou(b, (x, ar1 @@ Clou(n, env))::env')
        | _ -> CApp(op2 m', ar1 @@ Clou(n, env))
    )
  | Clou(Clou(t, env1), env2) -> gen_aux @@ Clou(t, env1 @ env2)
