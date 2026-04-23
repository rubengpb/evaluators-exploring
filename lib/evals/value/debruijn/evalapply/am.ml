open Core.Syntax
open Core.Utils
open Value_utils
open Bv

let rec am = function
  | DBApp (m, n) ->
    let m' = bv m in (
      match m' with
          | DBAbs b ->
            let n' = bv n in
            if is_dbvalue n' then
              am @@ subst_db (bv n) 0 b
            else
              DBApp(m', n')
          | _ -> DBApp (am m', bv n)
    )
  | DBAbs b -> DBAbs (am b)
  | t -> t
