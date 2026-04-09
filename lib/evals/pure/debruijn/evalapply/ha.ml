open Core.Syntax
open Core.Utils
open Bv

let rec ha = function
  | DBApp (m, n) ->
    let m' = bv m in (
      match m' with
          | DBAbs b -> ha @@ subst_db (ha n) 0 b
          | _ -> DBApp (ha m', ha n)
    )
  | DBAbs b -> DBAbs (ha b)
  | t -> t
