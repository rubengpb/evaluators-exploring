open Core.Syntax
open Core.Utils
open Cl_utils
open Core.Forms

let rec step_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) t =
  let step_gen_aux = step_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) in
  match t with
  | Clou(CVar x, []) -> CVar x
  | Clou(CVar x, (y, n)::env) ->
    if x = y then n
    else step_gen_aux (Clou(CVar x, env))
  | CAbs(x, b) when not (is_la b) ->
    let b' = la b in
    CAbs(x, b')
  | Clou(CAbs(x, b), env) ->
    let var = x ^ "*" ^ string_of_int (cl_fresh ()) in
    let b' = la @@ Clou(b, (x, CVar var)::env) in
      CAbs(var, b')
  | CApp(m, n) when not (is_op1 m) ->
    let m' = op1 m in
    CApp(m', n)
  | CApp(v, n) when not (is_ar1 n) ->
    let n' = ar1 n in
    CApp(v, n')
  | CApp (CAbs(x, b), n) ->
    Clou(b, [(x, n)])
  | CApp (Clou(CAbs(x, b), env), n) ->
    Clou(b, (x, n)::env)
  | CApp(m, n) when not (is_op2 m) ->
    let m' = op2 m in
    CApp(m', n)
  | CApp(v, n) when not (is_ar2 n) ->
    let n' = ar2 n in
    CApp(v, n')
  | Clou(CApp(m, n), env) ->
    step_gen_aux (CApp(Clou(m, env), Clou(n, env)))
  | Clou(Clou(t, env1), env2) ->
    step_gen_aux (Clou(t, env1 @ env2))
  | _ -> failwith "Gen: No redex found!"


let string_of_term_ss_gen = string_of_cterm

let rec gen is_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) t =
  if is_gen t then t
  else (
    print_endline @@ string_of_term_ss_gen t;
    let step_gen_aux = step_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) in
    let gen_aux = gen is_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) in
    t |> step_gen_aux |> gen_aux
  )
