open Core.Syntax
open Evalapply.Bv

let rec bodies = function
    | DBVar _ as v -> v
    | FDBVar _ as v -> v
    | DBAbs b ->
      let b' = bodies @@ bv b in
      DBAbs b'
  | DBApp (m, n) -> DBApp (bodies m, bodies n)

let rbbv t = bodies @@ bv t
