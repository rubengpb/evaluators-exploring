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

let rec list_of_dbterm_aux = function
  | DBVar 0 -> Some []
  | DBApp (DBApp (DBVar 1, x), rest) -> (
        match list_of_dbterm_aux rest with
        | Some xs -> Some (x :: xs)
        | None -> None)
  | _ -> None

let rec list_of_cterm_aux c n = function
  | CVar b ->
      if b = n then Some []
      else None
  | CApp (CApp (CVar f, x), rest) ->
      if f = c then
        match list_of_cterm_aux c n rest with
        | Some xs -> Some (x :: xs)
        | None -> None
      else None
  | _ -> None

let list_of_pterm = function
  | Abs (c, Abs (n, body)) ->
      list_of_pterm_aux c n body
  | _ -> None

let list_of_dbterm = function
  | DBAbs (DBAbs body) ->
      list_of_dbterm_aux body
  | _ -> None

let list_of_cterm = function
  | CAbs (c, CAbs (n, body)) ->
      list_of_cterm_aux c n body
  | _ -> None

let list_of_term = function
  | TPure t -> (
      match list_of_pterm t with
      | Some xs -> Some (List.map (fun x -> TPure x) xs)
      | None -> None)
  | TDeBruijn t -> (
      match list_of_dbterm t with
      | Some xs -> Some (List.map (fun x -> TDeBruijn x) xs)
      | None -> None)
  | TClousure t -> (
      match list_of_cterm t with
      | Some xs -> Some (List.map (fun x -> TClousure x) xs)
      | None -> None)
