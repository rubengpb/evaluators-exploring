open Core.Syntax
open Bv

let rec bodies = function
    | Var x -> Var x
    | Abs (x, b) ->
      let b' = bodies @@ eval_bv b in
      Abs(x, b')
  | App (m, n) -> App (bodies m, bodies n)

let eval_rbbv t = bodies @@ eval_bv t
