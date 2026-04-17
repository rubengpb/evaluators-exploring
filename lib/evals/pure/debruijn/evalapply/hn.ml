open Core.Syntax
open Core.Utils
open He

let rec hn = function
  | DBApp (m, n) ->
    let m' = he m in (
      match m' with
          | DBAbs b -> hn @@ subst_db m 0 b
          | _ -> DBApp (hn m', hn n)
    )
  | DBAbs b -> DBAbs (hn b)
  | t -> t
