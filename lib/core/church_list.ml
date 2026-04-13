open Syntax

let rec pterm_of_list lst =
  let c = Var "c" in
  let n = Var "n" in
  let rec build = function
    | [] -> n
    | x :: xs -> App(App (c, x), build xs)
  in
  Abs ("c", Abs ("n", build lst))

let rec list_of_pterm_aux c n = function
  | Var b ->
      if b = n then Some []
      else None

  | App (App (Var f, x), rest) ->
      if f = c then
        match list_of_pterm_aux c n rest with
        | Some xs -> Some (x :: xs)
        | None -> None
      else None

  | _ -> None

let list_of_pterm = function
  | Abs (c, Abs (n, body)) ->
      list_of_pterm_aux c n body
  | _ -> None

let list_of_term = function
  | TPure t -> list_of_pterm t
  | _ -> None
