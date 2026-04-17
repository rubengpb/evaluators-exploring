open Core.Syntax
open Core.Utils

let rec ho = function
  | DBApp (m, n) ->
    let m' = ho m in (
      match m' with
          | DBAbs b -> ho @@ subst_db (ho n) 0 b
          | _ -> DBApp (m', n)
    )
  | DBAbs b -> DBAbs (ho b)
  | t -> t
