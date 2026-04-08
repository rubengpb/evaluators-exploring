open Core.Syntax
open Core.Utils
open Bn

let rec hr = function
  | App (t1, t2) ->
    let t1' = bn t1 in (
      match t1' with
          | Abs (x, body) -> hr @@ subst t2 x body
          | _ -> App (hr t1', t2)
    )
  | Abs(x, t) -> Abs (x, hr t)
  | Var x -> Var x
