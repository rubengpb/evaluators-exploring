open Core.Syntax
open Core.Utils
open Value_utils

let rec bv = function
  | Var x as v -> v
  | Abs (x, b) as abs -> abs
  | App (m, n) -> apply (bv m) (bv n)
and apply m' n' =
  match n' with
  | App(_,_) -> App(m',n')
  | _ -> (
    match m' with
        | Abs (x, b) -> bv @@ subst n' x b
        | _ -> App(m',n')
    )
  (* match m with *)
  (*   | Abs (x, b) -> *)
  (*     if is_value n *)
  (*     then bv @@ subst n x b *)
  (*     else App(m, n) *)
  (*   | _ -> App(m, n) *)
  (* match m, n with *)
  (*   | Abs(_,_), App(_,_) | _, App(_, _) | _, App(_, _)  -> *)
  (*     App(m,n) *)
  (*   | Abs(x, b), _ -> bv @@ subst n x b *)
