open Core.Syntax
open Evalapply.Bv

let rec bodies2 = function
    | Var x -> Var x
    | Abs (x, b) ->
      let b' = bodies2 @@ bv b in
      Abs(x, b')
  | App (m, n) -> App (bodies2 m, n)

let rbam t = bodies2 @@ bv t
