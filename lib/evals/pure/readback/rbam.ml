open Core.Syntax
open Bv

let rec bodies2 = function
    | Var x -> Var x
    | Abs (x, b) ->
      let b' = bodies2 @@ eval_bv b in
      Abs(x, b')
  | App (m, n) -> App (bodies2 m, n)

let eval_rbam t = bodies2 @@ eval_bv t
