open Core.Syntax
open Evalapply.Bv

let rec bodies2 = function
    | CVar x -> CVar x
    | CAbs (x, b) ->
      let b' = bodies2 @@ bv b in
      CAbs(x, b')
  | CApp (m, n) -> CApp (bodies2 m, n)
  | _ -> failwith "ERROR: Bodies2 readback receives a Clou"

let rbam t = bodies2 @@ bv t
