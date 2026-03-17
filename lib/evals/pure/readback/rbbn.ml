open Core.Syntax
open He

let rec args = function
    | Var x -> Var x
    | Abs (x, b) ->
      let b' = args b in
      Abs(x, b')
  | App (m, n) -> App (args m, args @@ eval_he n)

let eval_rbbn t = args @@ eval_he t
