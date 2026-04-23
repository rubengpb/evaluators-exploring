open Core.Syntax
open Core.Utils
open Value_utils

let rec gen la op1 ar1 op2 ar2 t =
  let gen_aux = gen la op1 ar1 op2 ar2 in
  match t with
    | DBApp (m, n) ->
      let m' = op1 m in (
        match m' with
            | DBAbs b ->
              let n' = ar1 n in
              if is_dbvalue n' then
                gen_aux @@ subst_db (ar1 n) 0 b
              else
                DBApp(m', n')
            | _ -> DBApp (op2 m',  ar2 n)
      )
    | DBAbs b -> DBAbs (la b)
    | t -> t
