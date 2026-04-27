open Core.Syntax

let rec gen la ar2 = function
  | CVar x -> CVar x
  | CAbs (x, b) ->
    let b' = la b in
    CAbs(x, b')
  | CApp (m, n) -> CApp (gen la ar2 m, ar2 n)
  | _ -> failwith "ERROR: Generic Readback receive a Clou term"
