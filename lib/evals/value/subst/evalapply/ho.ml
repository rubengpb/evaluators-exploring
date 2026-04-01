open Core.Syntax
open Core.Utils
open Value_utils

let rec ho = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, ho b)
  | App (m, n) -> apply (ho m) n
and apply m n =
  match m with
    | Abs (x, b) ->
      if is_value n
      then ho @@ subst (ho n) x b
      else App(m, n)
    | _ -> App(m, n)
