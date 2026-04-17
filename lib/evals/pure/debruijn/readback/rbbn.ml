open Core.Syntax
open Evalapply.He

let rec args = function
    | FDBVar x as v -> v
    | DBVar x as v -> v
    | DBAbs b ->
      let b' = args b in
      DBAbs b'
  | DBApp (m, n) -> DBApp (args m, args @@ he n)

let rbbn t = args @@ he t
