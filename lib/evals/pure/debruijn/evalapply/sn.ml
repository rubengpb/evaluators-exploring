open Core.Syntax
open Core.Utils
open Bv

let rec sn = function
  | DBApp (m, n) ->
    let m' = bv m in (
      match m' with
          | DBAbs b -> sn @@ subst_db (bv n) 0 b
          | _ -> DBApp (sn m', sn n)
    )
  | DBAbs b -> DBAbs (sn b)
  | t -> t
