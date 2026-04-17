open Core.Syntax
open Core.Utils
open Ho

let rec bs = function
  | DBApp (m, n) ->
    let m' = ho m in (
      match m' with
          | DBAbs b -> bs @@ subst_db (ho n) 0 b
          | _ -> DBApp (bs m', bs n)
    )
  | DBAbs b -> DBAbs (bs b)
  | t -> t
