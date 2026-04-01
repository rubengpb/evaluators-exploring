open Core.Syntax
open Core.Utils

let rec gen la op1 ar1 op2 ar2 t =
  let gen_aux = gen la op1 ar1 op2 ar2 in
  match t with
    | App (t1, t2) ->
      let t1' = op1 t1 in (
        match t1' with
            | Abs (x, body) -> gen_aux @@ subst (ar1 t2) x body
            | _ -> App (op2 t1',  ar2 t2)
      )
    | Abs (x, body) -> Abs(x, la body)
    | Var x -> Var x
