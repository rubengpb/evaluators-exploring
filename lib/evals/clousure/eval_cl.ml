open Core.Syntax

let rec eval_cl = function
  | Clou (CVar x as v, Nihil) -> v
  | Clou (CVar x as v, Bind (y, v', cont)) ->
    if x = y then v'
    else eval_cl @@ Clou (v, cont)
  | Clou (CAbs (x,b), cont) -> Clou (CAbs(x, b), cont)
  | Clou (CApp (t1, t2), cont) ->
    eval_cl @@ CApp( eval_cl @@ Clou (t1, cont), eval_cl @@ Clou(t2, cont))
  | CApp (Clou(CAbs(x, b),cont), v) -> eval_cl @@ Clou(b, Bind(x, v, cont))
  | CApp (_, _) as t -> t
  | _ -> failwith "Error: the cterm cannot be reduced"

  let rec pure_of_clousure = function
  | CVar x -> Var x
  | CAbs (x, b) -> Abs (x, pure_of_clousure b)
  | CApp (t1, t2) -> App (pure_of_clousure t1, pure_of_clousure t2)
  | Clou (t, Nihil) -> pure_of_clousure t
  | Clou (t, Bind(_, _, _)) -> failwith "Error: context non empthy"

let rec clousure_of_pure_aux = function
    | Var x -> CVar x
    | Abs (x, b) -> CAbs (x, clousure_of_pure_aux b)
    | App (t1, t2) -> CApp (clousure_of_pure_aux t1, clousure_of_pure_aux t2)

let clousure_of_pure t =
  Clou (clousure_of_pure_aux t, Nihil)

let eval_cl_pure t =
  t |> clousure_of_pure |> eval_cl |> pure_of_clousure
