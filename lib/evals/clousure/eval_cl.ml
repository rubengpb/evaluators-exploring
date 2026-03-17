open Core.Syntax
open Core.Utils

let rec append_cntx c1 c2 =
  match c1 with
  | Nihil -> c2
  | Bind (x, v, c) -> Bind (x, v, append_cntx c c2)

let rec clean_cntx x cntx =
  match cntx with
    | Nihil -> Nihil
    | Bind (y, t, c) ->
      if y = x then clean_cntx x c
      else Bind (y, t, clean_cntx x c)

let rec eval_cl cntx t =
  match t, cntx with
  | CVar x, Nihil -> CVar x
  | CVar x, Bind (y, v', c) ->
    if x = y then v'
    else eval_cl c (CVar x)
  | CAbs (x, b), c ->
    Clou (CAbs(x, b), c)
  | CApp (t1, t2), cont ->
    apply (eval_cl cont t1) (eval_cl cont t2)
  | Clou (t , c1), c2 -> eval_cl (append_cntx c1 c2) t
  and apply f v =
    match f with
    | Clou (CAbs (x, b), c) -> eval_cl (Bind (x, v, c)) b
    | _ -> CApp (f, v)

let rec pure_of_clousure = function
  | CVar x -> Var x
  | CAbs (x, b) -> Abs (x, pure_of_clousure b)
  | CApp (t1, t2) -> App (pure_of_clousure t1, pure_of_clousure t2)
  | Clou (t, Nihil) -> pure_of_clousure t
  | Clou (t, env) ->
    apply_context env (pure_of_clousure t)
and apply_context env t =
  match env with
  | Nihil -> t
  | Bind (x, v, c) ->
      apply_context c (subst (pure_of_clousure (eval_cl Nihil v)) x t)



let rec clousure_of_pure = function
    | Var x -> CVar x
    | Abs (x, b) -> CAbs (x, clousure_of_pure b)
    | App (t1, t2) -> CApp (clousure_of_pure t1, clousure_of_pure t2)

let eval_cl_pure t =
  t |> clousure_of_pure |> (eval_cl Nihil) |> pure_of_clousure
