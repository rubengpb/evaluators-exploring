open Core.Syntax
open Evalapply.Bv

let rec bodies = function
    | CVar x -> CVar x
    | CAbs (x, b) ->
      let b' = bodies @@ bv b in
      CAbs(x, b')
  | CApp (m, n) -> CApp (bodies m, bodies n)
  | _ -> failwith "ERROR: Bodies readback receives a Clou"

let rbbv t = bodies @@ bv t
