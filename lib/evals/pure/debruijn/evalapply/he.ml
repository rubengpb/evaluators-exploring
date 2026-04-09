open Core.Syntax
open Core.Utils

let rec he = function
  | DBApp (m, n) ->
    let m' = he m in (
      match m' with
          | DBAbs b -> he @@ subst_db n 0 b
          | _ -> DBApp (m', n)
    )
  | DBAbs b -> DBAbs (he b)
  | t -> t
