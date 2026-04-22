open Core.Syntax
open Core.Utils
open Core.Forms

let rec step_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) t =
  failwith "TODO"

let string_of_term_ss_gen = string_of_cterm

let rec gen is_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) t =
  if is_gen t then t
  else (
    print_endline @@ string_of_term_ss_gen t;
    let step_gen_aux = step_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) in
    let gen_aux = gen is_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) in
    t |> step_gen_aux |> gen_aux
  )
