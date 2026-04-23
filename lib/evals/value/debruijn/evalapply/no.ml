open Core.Syntax
open Core.Utils
open Value_utils
open Bn

let rec no = function
  | DBApp (m, n) ->
    let m' = bn m in (
      match m' with
      | DBAbs b ->
        if is_dbvalue n then
          no @@ subst_db n 0 b
        else
          DBApp(m', n)
      | _ -> DBApp (no m', no n)
    )
  | DBAbs b -> DBAbs (no b)
  | t -> t
