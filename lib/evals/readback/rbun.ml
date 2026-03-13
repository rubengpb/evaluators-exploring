open Core.Syntax
open Bv

let rec bodies3 = function
    | Var x -> Var x
    | Abs (x, b) ->
      let b' = eval_bv b in
      Abs(x, b')
  | App (m, n) -> App (bodies3 m, bodies3 n)

let eval_rbun t = bodies3 @@ eval_bv t
