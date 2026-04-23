open Core.Syntax
open Evalapply.He

let rec args = function
  | Var x -> Var x
  | Abs (x, b) ->
    let b' = args b in
    Abs(x, b')
  | App (m, n) -> App (args m, args @@ he n)

let rbbn t = args @@ he t
