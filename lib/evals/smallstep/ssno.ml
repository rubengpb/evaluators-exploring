open Core.Syntax
open Core.Utils
open Core.Forms


let rec step_left_outer = function
  | Abs (x, body) -> Abs(x, step_left_outer body)
  | App (Abs (x, body), arg) -> subst arg x body
  | App (Var x, arg) -> App (Var x, step_left_outer arg)
  | App (t1, t2) -> App(step_left_outer t1, t2)
  | t -> t (* Variables *)
