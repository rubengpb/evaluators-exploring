open Core.Syntax
open Evalapply.Bn

let rec rn = function
  | CVar x -> CVar x
  | CAbs (x, b) ->
    let b' = rn @@ bn b in
    CAbs(x, b')
  | CApp (m, n) -> CApp (rn m, rn @@ bn n)
  | _ -> failwith "ERROR: Rn readback receives a Clou"

let rbno t = rn @@ bn t
