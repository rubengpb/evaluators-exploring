open Syntax

let rec pterm_of_int_aux = function
  | 0 -> Var "z"
  | n -> App (Var "s", pterm_of_int_aux (n - 1))

let pterm_of_int n =
  Abs ("s", Abs ("z", pterm_of_int_aux n))

let term_of_int n =
  TPure (pterm_of_int n)

let rec int_of_pterm_aux x y = function
  | Var b ->
      if b = y then Some 0 else None
  | App (Var a, e) ->
      if x = a then
        match int_of_pterm_aux x y e with
        | Some n -> Some (n + 1)
        | None -> None
      else None
  | _ -> None

let rec int_of_cterm_aux x y = function
  | CVar b ->
      if b = y then Some 0 else None
  | CApp (CVar a, e) ->
      if x = a then
        match int_of_cterm_aux x y e with
        | Some n -> Some (n + 1)
        | None -> None
      else None
  | _ -> None

let rec int_of_dbterm_aux = function
  | DBVar 0 -> Some 0
  | DBApp (DBVar 1, e) -> (
        match int_of_dbterm_aux e with
        | Some n -> Some (n + 1)
        | None -> None)
  | _ -> None

let int_of_pterm = function
  | Abs (x, Abs (y, e)) -> int_of_pterm_aux x y e
  | _ -> None

let int_of_dbterm = function
  | DBAbs (DBAbs e) -> int_of_dbterm_aux e
  | _ -> None

let int_of_cterm = function
  | CAbs (x, (CAbs(y, e))) -> int_of_cterm_aux x y e
  | _ -> None

let int_of_term = function
  | TPure t -> int_of_pterm t
  | TDeBruijn  t-> int_of_dbterm t
  | TClousure t -> int_of_cterm t
