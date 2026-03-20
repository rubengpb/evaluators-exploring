open Core.Syntax

let rec eval_cl_bn ct =
  match ct with
    | Clou (t, ctx) -> (
    match t, ctx with
      | CVar x, [] -> ct
      | CVar x as var , (y, n)::c ->
        if x = y then eval_cl_bn n
        else eval_cl_bn (Clou (var, c))
      | CAbs (_, _), _-> ct
      | CApp (m, n), c ->
        apply (eval_cl_bn (Clou(m,c))) (Clou (n, c))
      | _ -> failwith "[Error] Evaluation of CallByName with Clousure"
    )
    | _ -> failwith "[Error] Incorrect input in Clousure CallByName evaluation"
and apply m n =
  match m with
    | Clou (CAbs(x, b), c) -> eval_cl_bn (Clou(b, (x, n)::c))
    | _ -> CApp(m, n)
