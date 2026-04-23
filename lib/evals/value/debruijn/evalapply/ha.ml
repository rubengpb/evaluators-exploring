open Core.Syntax
open Core.Utils
open Value_utils
open Bv

let rec ha = function
  | DBApp (m, n) ->
    let m' = bv m in (
      match m' with
          | DBAbs b ->
            let n' = ha n in
            if is_dbvalue n' then
              ha @@ subst_db (ha n) 0 b
            else
              DBApp(m', n')
          | _ -> DBApp (ha m', ha n)
    )
  | DBAbs b -> DBAbs (ha b)
  | t -> t
