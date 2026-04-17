open Syntax

let is_abs = function
  | Abs _ -> true
  | _ -> false

let is_var = function
  | Var _ -> true
  | _ -> false

let is_value t = (is_abs t) || (is_var t)

let rec spine t =
  match t with
  | App (t1, t2) ->
      let (h, args) = spine t1 in
      (h, args @ [t2])
  | _ -> (t, [])

let is_neu t =
  match spine t with
  | Var _, args when args <> [] -> true
  | Var _, [] -> false
  | _ -> false

let rec is_nf t =
  match t with
  | Abs (_, body) -> is_nf body
  | _ ->
      (match spine t with
       | Var _, args -> List.for_all is_nf args
       | _ -> false)

let rec is_wnf t =
  match t with
  | Abs (_, _) -> true
  | _ ->
      (match spine t with
       | Var _, args -> List.for_all is_wnf args
       | _ -> false)

let rec is_hnf t =
  match t with
  | Abs (_, body) -> is_hnf body
  | _ ->
      (match spine t with
       | Var _, _ -> true
       | _ -> false)

let is_whnf t =
  match t with
  | Abs (_, _) -> true
  | _ ->
      (match spine t with
       | Var _, _ -> true
       | _ -> false)

let rec is_vhnf t =
  match t with
  | Abs (_, body) -> is_vhnf body
  | _ ->
      (match spine t with
       | Var _, args -> List.for_all is_wnf args
       | _ -> false)

let is_dbabs = function
  | DBAbs _ -> true
  | _ -> false

let is_dbvar = function
  | DBVar _ -> true
  | FDBVar _ -> true
  | _ -> false

let is_dbvalue t = (is_dbabs t) || (is_dbvar t)

let rec dbspine t =
  match t with
  | DBApp (t1, t2) ->
      let (h, args) = dbspine t1 in
      (h, args @ [t2])
  | _ -> (t, [])

let is_dbneu t =
  match dbspine t with
  | DBVar _, args when args <> [] -> true
  | DBVar _, [] -> false
  | _ -> false

let rec is_dbnf t =
  match t with
  | DBAbs body -> is_dbnf body
  | _ ->
      (match dbspine t with
       | DBVar _, args -> List.for_all is_dbnf args
       | FDBVar _, args -> List.for_all is_dbnf args
       | _ -> false)

let rec is_dbwnf t =
  match t with
  | DBAbs _ -> true
  | _ ->
      (match dbspine t with
       | DBVar _, args -> List.for_all is_dbwnf args
       | FDBVar _, args -> List.for_all is_dbwnf args
       | _ -> false)

let rec is_dbhnf t =
  match t with
  | DBAbs body -> is_dbhnf body
  | _ ->
      (match dbspine t with
       | DBVar _, _ -> true
       | FDBVar _, _ -> true
       | _ -> false)

let is_dbwhnf t =
  match t with
  | DBAbs _ -> true
  | _ ->
      (match dbspine t with
       | DBVar _, _ -> true
       | FDBVar _, _ -> true
       | _ -> false)

let rec is_dbvhnf t =
  match t with
  | DBAbs body -> is_dbvhnf body
  | _ ->
      (match dbspine t with
       | DBVar _, args -> List.for_all is_dbwnf args
       | FDBVar _, args -> List.for_all is_dbwnf args
       | _ -> false)
