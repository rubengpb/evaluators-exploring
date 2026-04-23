open Core.Syntax
open Evalapply.Bv

let rec bodies3 = function
  | Var x -> Var x
  | Abs (x, b) ->
    let b' = bv b in
    Abs(x, b')
  | App (m, n) -> App (bodies3 m, bodies3 n)

let rbun t = bodies3 @@ bv t
