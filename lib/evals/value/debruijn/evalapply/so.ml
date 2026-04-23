open Core.Syntax
open Core.Utils
open Value_utils
open Ho

let rec so = function
  | DBApp (m, n) ->
    let m' = ho m in (
      match m' with
          | DBAbs b ->
            let n' = so n in
            if is_dbvalue n' then
            so @@ subst_db n' 0 b
            else
              DBApp(m', n')
          | _ -> DBApp (so m', so n)
    )
  | DBAbs b -> DBAbs (so b)
  | t -> t
