open Core.Syntax
open Core.Utils
open Core.Forms

let rec eval_ss is_value step t =
  if is_value t then t
  else (
    print_endline (string_of_term t);
    t |> step |> (eval_ss is_value step)
  )

let rec step_left_outer = function
  | Abs (x, body) -> Abs(x, step_left_outer body)
  | App (Abs (x, body), arg) -> subst arg x body
  | App (Var x, arg) -> App (Var x, step_left_outer arg)
  | App (t1, t2) -> App(step_left_outer t1, t2)
  | _ -> failwith "Value cannot be reducing"

let rec step_left_inner = function
  | Abs (x, body) -> Abs(x, step_left_inner body)
  | App (Var x, arg) -> App (Var x, step_left_inner arg)
  | App (Abs (x, body), arg) ->
    let is_body_nf = is_nf body in
    let is_arg_nf = is_nf arg in
    if  is_body_nf && is_arg_nf then subst arg x body
    else if is_body_nf then App(Abs(x, body), step_left_inner arg)
    else App(Abs(x, step_left_inner body), arg)
  | App (t1, t2) -> App(step_left_inner t1, t2)
  | _ -> failwith "Value cannot be reducing"

let eval_ssno t = eval_ss is_nf step_left_outer t
let eval_ssao t = eval_ss is_nf step_left_inner t
