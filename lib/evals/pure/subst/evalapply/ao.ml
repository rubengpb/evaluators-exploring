open Core.Syntax
open Core.Utils

let rec ao = function
  | App (t1, t2) ->
    let t1' = ao t1 in (
      match t1' with
        | Abs (x, body) -> ao @@ subst (ao t2) x body
        | _ -> App(t1', ao t2)
    )
  | Abs (x, t) -> Abs (x, ao t)
  | Var x -> Var x
