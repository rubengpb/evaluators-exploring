open Core.Syntax
open Core.Utils
open Ho

let rec bs = function
  | App (t1, t2) ->
    let t1' = ho t1 in (
      match t1' with
          | Abs (x, body) -> bs @@ subst (ho t2) x body
          | _ -> App (bs t1', bs t2)
    )
  | Abs(x, t) -> Abs (x, bs t)
  | Var x -> Var x
