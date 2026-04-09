open Core.Syntax
open Core.Utils

let rec bv = function
  | DBApp (m, n) ->
    let m' = bv m in (
      match m' with
          | DBAbs b -> bv @@ subst_db (bv n) 0 b
          | _ -> DBApp (m', bv n)
    )
  | t -> t
