open Core.Syntax
open Core.Subst

let rec eval_ao = function
  | App (t1, t2) ->
      let t1' = eval_ao t1 in
      (match t1' with
        | Abs (x, body) -> eval_ao @@ subst body x @@ eval_ao t2
        | _ -> App(t1', eval_ao t2)
      )

  | Abs (x, t) ->
      Abs (x, eval_ao t)

  | Var x -> Var x
