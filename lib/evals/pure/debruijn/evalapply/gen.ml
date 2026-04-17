open Core.Syntax
open Core.Utils

let rec gen la op1 ar1 op2 ar2 t =
  let gen_aux = gen la op1 ar1 op2 ar2 in
  match t with
    | DBApp (m, n) ->
      let m' = op1 m in (
        match m' with
            | DBAbs b -> gen_aux @@ subst_db (ar1 n) 0 b
            | _ -> DBApp (op2 m',  ar2 n)
      )
    | DBAbs b -> DBAbs (la b)
    | t -> t
