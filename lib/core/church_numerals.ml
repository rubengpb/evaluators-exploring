open Syntax

let rec int_of_term_aux = function
  | 0 -> Var "z"
  | n -> App (Var "s", int_of_term_aux (n - 1))

let int_of_term n =
  Abs ("s", Abs ("z", int_of_term_aux n))

let rec term_of_int_aux x y = function
  | Var b ->
      if b = y then Some 0 else None
  | App (Var a, e) ->
      if x = a then
        match term_of_int_aux x y e with
        | Some n -> Some (n + 1)
        | None -> None
      else None
  | _ -> None

let term_of_int = function
  | Abs (x, Abs (y, e)) -> term_of_int_aux x y e
  | _ -> None
