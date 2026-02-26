open Syntax

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
