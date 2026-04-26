open Core.Syntax
open Core.Utils
open Value_utils
open Bv

let rec sn = function
  | DBApp (m, n) ->
    let m' = bv m in (
      match m' with
          | DBAbs b ->
            let n' = bv n in
            if is_dbvalue n' then
              sn @@ subst_db n' 0 b
            else
              DBApp(m', n')
          | _ -> DBApp (sn m', sn n)
    )
  | DBAbs b -> DBAbs (sn b)
  | t -> t
