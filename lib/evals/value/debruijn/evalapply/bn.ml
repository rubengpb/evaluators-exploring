open Core.Syntax
open Core.Utils
open Value_utils

let rec bn = function
  | DBApp (m, n) ->
    let m' = bn m in (
      match m' with
          | DBAbs b ->
            if is_dbvalue n then
              bn @@ subst_db n 0 b
            else
              DBApp(m', n)
          | _ -> DBApp (m', n)
    )
  | t -> t
