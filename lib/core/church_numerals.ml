open Syntax

let rec term_of_int_aux = function
  | 0 -> Var "z"
  | n -> App (Var "s", term_of_int_aux (n - 1))

let term_of_int n =
  Abs ("s", Abs ("z", term_of_int_aux n))

let rec int_of_term_aux x y = function
  | Var b ->
      if b = y then Some 0 else None
  | App (Var a, e) ->
      if x = a then
        match int_of_term_aux x y e with
        | Some n -> Some (n + 1)
        | None -> None
      else None
  | _ -> None

let int_of_term = function
  | Abs (x, Abs (y, e)) -> int_of_term_aux x y e
  | _ -> None
