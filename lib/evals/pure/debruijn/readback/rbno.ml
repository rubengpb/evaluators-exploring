open Core.Syntax
open Evalapply.Bn

let rec rn = function
  | DBVar x as v -> v
  | FDBVar x as v -> v
  | DBAbs b ->
    let b' = rn @@ bn b in
    DBAbs b'
  | DBApp (m, n) -> DBApp (rn m, rn @@ bn n)

let rbno t = rn @@ bn t
