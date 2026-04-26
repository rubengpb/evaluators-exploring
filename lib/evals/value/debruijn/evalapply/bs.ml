open Core.Syntax
open Core.Utils
open Value_utils
open Ho

let rec bs = function
  | DBApp (m, n) ->
    let m' = ho m in (
      match m' with
          | DBAbs b ->
            let n' = ho n in
            if is_dbvalue n' then
              bs @@ subst_db n' 0 b
            else
              DBApp(m', n')
          | _ -> DBApp (bs m', bs n)
    )
  | DBAbs b -> DBAbs (bs b)
  | t -> t
