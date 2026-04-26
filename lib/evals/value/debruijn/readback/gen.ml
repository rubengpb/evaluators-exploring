open Core.Syntax

let rec gen la ar2 = function
  | DBVar _ as v -> v
  | FDBVar _ as v -> v
  | DBAbs b ->
    let b' = la b in
    DBAbs b'
  | DBApp (m, n) -> DBApp (gen la ar2 m, ar2 n)
