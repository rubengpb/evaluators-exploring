open Core.Syntax
open Core.Utils
open Core.Forms

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
  | t -> t (* Variables *)
