open Syntax

type typ =
  | TVar of string
  | TFun of typ * typ

let counter = ref 0

let fresh_type_var () =
  let v = !counter in
  incr counter;
  TVar ("t" ^ string_of_int v)

type subst = (string * typ) list

let rec apply_subst s t =
  match t with
  | TVar v ->
      (match List.assoc_opt v s with
       | Some t -> t
       | None -> TVar v)
  | TFun (t1,t2) ->
      TFun (apply_subst s t1, apply_subst s t2)

let rec occurs v t =
  match t with
  | TVar x -> x = v
  | TFun(t1,t2) -> occurs v t1 || occurs v t2

let rec unify t1 t2 =
  match (t1,t2) with
  | (TVar v, t) | (t, TVar v) ->
      if t = TVar v then []
      else if occurs v t then failwith "Error: term without type"
      else [(v,t)]
  | (TFun(a1,b1), TFun(a2,b2)) ->
      let s1 = unify a1 a2 in
      let b1' = apply_subst s1 b1 in
      let b2' = apply_subst s1 b2 in
      let s2 = unify b1' b2' in
      s2 @ s1

type env = (string * typ) list

let rec infer env term =
  match term with

  | Var x ->
      (try (List.assoc x env, [])
       with Not_found -> failwith "Error: term without type")

  | Abs (x,t) ->
      let tv = fresh_type_var () in
      let (t_body, s) = infer ((x,tv)::env) t in
      (TFun(apply_subst s tv, t_body), s)

  | App (t1,t2) ->
      let (ty1,s1) = infer env t1 in
      let (ty2,s2) = infer env t2 in
      let tv = fresh_type_var () in
      let s3 =
        unify (apply_subst s2 ty1)
              (TFun(ty2, tv))
      in
      (apply_subst s3 tv, s3 @ s2 @ s1)

let infer_type term =
  counter := 0;
  let (ty, subst) = infer [] term in
  apply_subst subst ty

let rec string_of_type t =
  match t with
  | TVar v -> v
  | TFun (TFun (_, _) as tf, t) ->
    "(" ^ string_of_type tf ^ ") -> " ^ string_of_type t
  | TFun (t1, t2) ->
    string_of_type t1 ^ " -> " ^ string_of_type t2
