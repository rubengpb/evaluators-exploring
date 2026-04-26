open Core.Syntax
open Core.Utils
open Core.Forms

let rec step_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) t =
  let step_gen_aux = step_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) in
  match t with
    | DBApp(m, n) when not (is_op1 m) ->
      let m' = op1 m in
      DBApp(m', n)
    | DBApp(v, n) when (is_dbabs v) && not (is_ar1 n) ->
      let n' = ar1 n in
      DBApp(v, n')
    | DBApp(DBAbs b, v) ->
      subst_db v 0 b
    | DBApp(m, n) when not (is_op2 m) ->
      let m' = ar2 m in
      DBApp(m', n)
    | DBApp(v, n) when not (is_ar2 n) ->
      let n' =  n in
      DBApp(v, n')
    | DBAbs b when not (is_la b) ->
      let b' = step_gen_aux b in
      DBAbs b'
    | _ -> failwith "Gen: No redex!"

let string_of_term_ss_gen = string_of_dbterm

let rec gen is_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) t =
  if is_gen t then t
  else (
    print_endline @@ string_of_term_ss_gen t;
    let step_gen_aux = step_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) in
    let gen_aux = gen is_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) in
    t |> step_gen_aux |> gen_aux
  )
