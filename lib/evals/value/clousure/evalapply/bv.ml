open Core.Syntax

let rec clasicall_bv ctxt t =
  match t, ctxt with
  | CVar x, [] -> CVar x
  | CVar x, (y, v') :: c ->
    if x = y then v'
    else clasicall_bv c (CVar x)
  | CAbs (x, b), c ->
    Clou (CAbs(x, b), c)
  | CApp (t1, t2), cont ->
    apply (clasicall_bv cont t1) (clasicall_bv cont t2)
  | _ -> failwith "[Error] Evaluation of CallByValue with Clousure"
  and apply f v =
    match f with
    | Clou (CAbs (x, b), c) -> clasicall_bv ((x, v)::c) b
    | _ -> CApp (f, v)

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
        | CAbs(x, b) -> bv @@ Clou(b, [(x, n')])
        | Clou(CAbs(x, b), env) -> bv @@ Clou(b, (x, n')::env)
        | _ -> CApp(m', n')
    )
  | Clou(CApp(m, n), env) ->
    let m' = bv @@ Clou(m, env) in
    let n' = bv @@ Clou(n, env) in (
      match m' with
        | CAbs(x, b) -> bv @@ Clou(b, [(x, n')])
        | Clou(CAbs(x, b), env) -> bv @@ Clou(b, (x, n')::env)
        | _ -> CApp(m', n')
    )
  (* | Clou(Clou(t, env1), env2) -> bv @@ Clou(t, env1 @ env2) *)
  | _ -> failwith "[Error] Evaluation of CallByValue wiht Clousure"
