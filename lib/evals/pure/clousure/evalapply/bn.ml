open Core.Syntax

let rec bn = function
  | CVar x as v -> v
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, n)::env) -> if x = y then bn n else bn @@ Clou(CVar x, env)
  | CAbs(_,_) as abs -> abs
  | Clou(CAbs(_,_),_) as abs -> abs
  | CApp(m, n) ->
    let m' = bn m in (
      match m' with
        | CAbs(x, b) -> bn @@ Clou(b, [(x, n)])
        | Clou(CAbs(x, b), env) -> bn @@ Clou(b, (x, n)::env)
        | _ -> CApp(m', n)
    )
  | Clou(CApp(m, n), env) ->
    let m' = bn @@ Clou(m, env) in (
      match m' with
        | CAbs(x, b) -> bn @@ Clou(b, [(x, Clou(n, env))])
        | Clou(CAbs(x, b), env') -> bn @@ Clou(b, (x, Clou(n, env))::env')
        | _ -> CApp(m', Clou(n, env))
    )
  (* | Clou(Clou(t, env1), env2) -> bn @@ Clou(t, env1 @ env2) *)
  | _ -> failwith "[Error] Evaluation of CallByName wiht Clousure"
