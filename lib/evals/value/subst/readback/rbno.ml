open Core.Syntax
open Evalapply.Bn

let rec rn = function
  | Var x -> Var x
  | Abs (x, b) ->
    let b' = rn @@ bn b in
    Abs(x, b')
  | App (m, n) -> App (rn m, rn @@ bn n)

let rbno t = rn @@ bn t
