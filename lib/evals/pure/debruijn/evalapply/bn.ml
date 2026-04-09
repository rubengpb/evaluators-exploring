open Core.Syntax
open Core.Utils

let rec bn = function
  | DBApp (m, n) ->
    let m' = bn m in (
      match m' with
          | DBAbs b -> bn @@ subst_db n 0 b
          | _ -> DBApp (m', n)
    )
  | t -> t
