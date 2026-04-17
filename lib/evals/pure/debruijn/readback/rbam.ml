open Core.Syntax
open Evalapply.Bv

let rec bodies2 = function
    | DBVar x as v -> v
    | FDBVar x as v -> v
    | DBAbs b ->
      let b' = bodies2 @@ bv b in
      DBAbs b'
  | DBApp (m, n) -> DBApp (bodies2 m, n)

let rbam t = bodies2 @@ bv t
