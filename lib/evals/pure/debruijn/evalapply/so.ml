open Core.Syntax
open Core.Utils
open Ho

let rec so = function
  | DBApp (m, n) ->
    let m' = ho m in (
      match m' with
          | DBAbs b -> so @@ subst_db (so n) 0 b
          | _ -> DBApp (so m', so n)
    )
  | DBAbs b -> DBAbs (so b)
  | t -> t
