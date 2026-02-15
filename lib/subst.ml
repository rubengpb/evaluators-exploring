open Syntax

let rec free_vars = function
  | Var x -> [x]
  | Abs (x, t) ->
      List.filter (fun y -> y <> x) (free_vars t)
  | App (t1, t2) ->
      free_vars t1 @ free_vars t2

let rec subst t x s =
  match t with
  | Var y ->
      if y = x then s else t

  | Abs (y, body) ->
      if y = x then
        t
      else
        Abs (y, subst body x s)

  | App (t1, t2) ->
      App (subst t1 x s, subst t2 x s)
