open Core.Syntax
open Core.Utils

let rec ho = function
  | App (t1, t2) ->
    let t1' = ho t1 in (
      match t1' with
          | Abs (x, body) -> ho @@ subst (ho t2) x body
          | _ -> App (t1', t2)
    )
  | Abs(x, t) -> Abs (x, ho t)
  | Var x -> Var x
