open Core.Syntax
open Core.Utils
open Value_utils
open He

let rec hn = function
  | DBApp (m, n) ->
    let m' = he m in (
      match m' with
          | DBAbs b ->
            if is_dbvalue n then
              hn @@ subst_db n 0 b
            else
              DBApp(m', n)
          | _ -> DBApp (hn m', hn n)
    )
  | DBAbs b -> DBAbs (hn b)
  | t -> t
