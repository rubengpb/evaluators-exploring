open Core.Syntax
open Core.Utils
open Value_utils
open He

let rec hn = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, hn b)
  | App (m, n) -> apply (he m) n
and apply m' n =
  match m' with
    | Abs (x, b) when is_value n ->
      hn @@ subst n x b
    | Abs _ -> App(m', n)
    | _ -> App(hn m', hn n)
