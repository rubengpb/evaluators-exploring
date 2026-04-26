open Core.Syntax
open Core.Utils
open Value_utils
open Bn

let rec hr = function
  | DBApp (m, n) ->
    let m' = bn n in (
      match m' with
          | DBAbs b ->
            if is_dbvalue n then
              hr @@ subst_db n 0 b
            else
              DBApp(m', n)
          | _ -> DBApp (hr m', n)
    )
  | DBAbs b -> DBAbs (hr b)
  | t -> t
