open Core.Syntax
open Evalapply.Bv

let rec bodies = function
  | Var x -> Var x
  | Abs (x, b) ->
    let b' = bodies @@ bv b in
    Abs(x, b')
  | App (m, n) -> App (bodies m, bodies n)

let rbbv t = bodies @@ bv t
