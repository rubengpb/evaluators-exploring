open Core.Syntax
open Core.Utils
open Value_utils

let rec he = function
  | DBApp (m, n) ->
    let m' = he m in (
      match m' with
          | DBAbs b ->
            if is_dbvalue n then
              he @@ subst_db n 0 b
            else
              DBApp(m', n)
          | _ -> DBApp (m', n)
    )
  | DBAbs b -> DBAbs (he b)
  | t -> t
