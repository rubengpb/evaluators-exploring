open Core.Syntax
open Evalapply.He

let rec args = function
    | CVar x -> CVar x
    | CAbs (x, b) ->
      let b' = args b in
      CAbs(x, b')
  | CApp (m, n) -> CApp (args m, args @@ he n)
  | _ -> failwith "ERROR: Args readback receives a Clou"

let rbbn t = args @@ he t
