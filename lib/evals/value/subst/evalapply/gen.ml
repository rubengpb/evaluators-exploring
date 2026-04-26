open Core.Syntax
open Core.Utils
open Value_utils

let rec gen la op1 ar1 op2 ar2 t =
  let gen_aux = gen la op1 ar1 op2 ar2 in
  match t with
    | App (n, m) ->
      let m' = op1 m in (
        match m' with
            | Abs (x, body) ->
              let n' = ar1 n in
              if is_value n' then
                gen_aux @@ subst n' x body
              else
                App(m', n')
            | _ -> App (op2 m',  ar2 n)
      )
    | Abs (x, body) -> Abs(x, la body)
    | Var x -> Var x
