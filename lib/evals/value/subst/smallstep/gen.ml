open Core.Syntax
open Core.Utils
open Core.Forms

let rec step_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) t =
  let step_gen_aux = step_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) in
  match t with
    | App(m, n) when not (is_op1 m) ->
      let m' = step_gen_aux m in
      App(m', n)
    | App(v, n) when (is_abs v) && not (is_ar1 n) ->
      let n' = step_gen_aux n in
      App(v, n')
    | App(Abs(x, b), v) when is_value v ->
      subst v x b
    | App(m, n) when not (is_op2 m) ->
      let m' = step_gen_aux m in
      App(m', n)
    | App(v, n) when not (is_ar2 n) ->
      let n' = step_gen_aux n in
      App(v, n')
    | Abs(x, b) when not (is_la b) ->
      let b' = step_gen_aux b in
      Abs(x, b')
    | _ -> failwith "Gen: Not redex!"

let string_of_term_ss_gen = string_of_pterm

let rec gen is_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) t =
  if is_gen t then t
  else (
    print_endline @@ string_of_term_ss_gen t;
    let step_gen_aux = step_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) in
    let gen_aux = gen is_gen (la, is_la) (op1, is_op1) (ar1,is_ar1) (op2, is_op2) (ar2, is_ar2) in
    t |> step_gen_aux |> gen_aux
  )
