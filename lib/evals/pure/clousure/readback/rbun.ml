open Core.Syntax
open Evalapply.Bv

let rec bodies3 = function
    | CVar x -> CVar x
    | CAbs (x, b) ->
      let b' = bv b in
      CAbs(x, b')
  | CApp (m, n) -> CApp (bodies3 m, bodies3 n)
  | _ -> failwith "ERROR: Bodies3 readback receives a Clou"

let rbun t = bodies3 @@ bv t
