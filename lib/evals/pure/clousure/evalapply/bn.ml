open Core.Syntax

let rec bn ct =
  match ct with
    | Clou (t, ctx) -> (
    match t, ctx with
      | CVar x, [] -> ct
      | CVar x as var , (y, n)::c ->
        if x = y then bn n
        else bn (Clou (var, c))
      | CAbs (_, _), _-> ct
      | CApp (m, n), c ->
        apply (bn (Clou(m,c))) (Clou (n, c))
      | _ -> failwith "[Error] Evaluation of CallByName with Clousure"
    )
    | _ -> failwith "[Error] Incorrect input in Clousure CallByName evaluation"
and apply m n =
  match m with
    | Clou (CAbs(x, b), c) -> bn (Clou(b, (x, n)::c))
    | _ -> CApp(m, n)
