open Core.Syntax
open Evalapply.Bv

let rec bodies3 = function
    | DBVar x as v -> v
    | FDBVar x as v -> v
    | DBAbs b ->
      let b' = bv b in
      DBAbs b'
  | DBApp (m, n) -> DBApp (bodies3 m, bodies3 n)

let rbun t = bodies3 @@ bv t
