open Core.Syntax
open Core.Utils
open Ho

let rec so = function
  | App (t1, t2) ->
    let t1' = ho t1 in (
      match t1' with
          | Abs (x, body) -> so @@ subst (so t2) x body
          | _ -> App (so t1', so t2)
    )
  | Abs(x, t) -> Abs (x, so t)
  | Var x -> Var x
