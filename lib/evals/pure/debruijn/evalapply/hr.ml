open Core.Syntax
open Core.Utils
open Bn

let rec hr = function
  | DBApp (m, n) ->
    let m' = bn n in (
      match m' with
          | DBAbs b -> hr @@ subst_db m 0 b
          | _ -> DBApp (hr m', n)
    )
  | DBAbs b -> DBAbs (hr b)
  | t -> t
