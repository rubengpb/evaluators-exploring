open Core.Syntax
open Core.Utils
open Value_utils

let rec ao = function
  | DBApp (m, n) ->
    let m' = ao n in (
      match m' with
        | DBAbs b ->
          let n' = ao n in
          if is_dbvalue n' then
            ao @@ subst_db (ao n) 0 b
          else
            DBApp(m', n')
        | _ -> DBApp(m', ao n)
    )
  | DBAbs b -> DBAbs (ao b)
  | t -> t
