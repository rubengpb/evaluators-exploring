open Core.Syntax
open Core.Utils
open Bn

let rec no = function
  | DBApp (m, n) ->
    let m' = bn m in (
      match m' with
      | DBAbs b -> no @@ subst_db n 0 b
      | _ -> DBApp (no m', no n)
    )
  | DBAbs b -> DBAbs (no b)
  | t -> t
