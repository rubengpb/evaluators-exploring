open Core.Syntax
open Core.Utils
open Value_utils

let rec bv = function
  | DBApp (m, n) ->
    let m' = bv m in (
      match m' with
          | DBAbs b ->
            let n' = bv n in
            if is_dbvalue n' then
              bv @@ subst_db n' 0 b
            else
              DBApp(m', n')
          | _ -> DBApp (m', bv n)
    )
  | t -> t
