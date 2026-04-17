open Core.Syntax
open Core.Utils
open Bv

let rec am = function
  | DBApp (m, n) ->
    let m' = bv m in (
      match m' with
          | DBAbs b -> am @@ subst_db (bv n) 0 b
          | _ -> DBApp (am m', bv n)
    )
  | DBAbs b -> DBAbs (am b)
  | t -> t
