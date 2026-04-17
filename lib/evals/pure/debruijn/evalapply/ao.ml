open Core.Syntax
open Core.Utils

let rec ao = function
  | DBApp (m, n) ->
    let m' = ao n in (
      match m' with
        | DBAbs b -> ao @@ subst_db (ao n) 0 b
        | _ -> DBApp(m', ao n)
    )
  | DBAbs b -> DBAbs (ao b)
  | t -> t
