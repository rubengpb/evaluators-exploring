open Core.Syntax
open Bn

let rec rn = function
  | Var x -> Var x
  | Abs (x, b) ->
    let b' = rn @@ eval_bn b in
    Abs(x, b')
  | App (m, n) -> App (rn m, rn @@ eval_bn n)

let eval_rbno t = rn @@ eval_bn t
