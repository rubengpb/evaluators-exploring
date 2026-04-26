open Core.Syntax
open Core.Utils
open Value_utils

let rec ho = function
  | DBApp (m, n) ->
    let m' = ho m in (
      match m' with
          | DBAbs b ->
            let n' = ho n in
            if is_dbvalue n' then
              ho @@ subst_db n' 0 b
            else
              DBApp(m', n')
          | _ -> DBApp (m', n)
    )
  | DBAbs b -> DBAbs (ho b)
  | t -> t
