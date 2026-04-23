open Core.Syntax
open Core.Utils
open Value_utils
open Ho

let rec so = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, so b)
  | App (m, n) -> apply (ho m) n
and apply m' n =
  match m' with
    | Abs (x, b) ->
      let n' = so n in
      if is_value n' then so @@ subst n' x b
      else App(m', n')
    | _ -> App(so m', so n)
