open Core.Syntax

let rec gen la ar2 = function
  | Var x -> Var x
  | Abs (x, b) ->
    let b' = la b in
    Abs(x, b')
  | App (m, n) -> App (gen la ar2 m, ar2 n)
