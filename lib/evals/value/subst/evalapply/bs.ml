open Core.Syntax
open Core.Utils
open Value_utils
open Ho

let rec bs = function
  | Var x as v -> v
  | Abs (x, b) -> Abs(x, bs b)
  | App (m, n) -> apply (ho m) n
and apply m' n =
  match m' with
    | Abs (x, b) ->
      let n' = ho n in
      if is_value n' then bs @@ subst n' x b
      else App(m', n')
    | _ -> App(bs m', bs n)
