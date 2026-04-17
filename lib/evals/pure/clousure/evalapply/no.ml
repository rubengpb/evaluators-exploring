open Core.Syntax
open Core.Utils
open Cl_utils
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
        | CAbs(x, b) -> no @@ Clou(b, [(x, n)])
        | Clou(CAbs(x, b), env) -> no @@ Clou(b, (x, n)::env)
        | _ -> let m'' = no m' in
          let n' = no n in
            CApp(m'', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = bn @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) -> no @@ Clou(b, [(x, Clou(n, env))])
        | Clou(CAbs(x, b), env') -> no @@ Clou(b, (x, Clou(n, env))::env')
        | _ -> let m'' = no m' in
          let n' = no @@ Clou(n, env) in
            CApp(m'', n')
    )
  (* | Clou(Clou(t, env1), env2) -> no @@ Clou(t, env1 @ env2) *)
  | _ -> failwith "[Error] Evaluation of NormalOrder wiht Clousure"
