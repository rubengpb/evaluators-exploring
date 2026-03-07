open Syntax

let rec term_to_string t =
  match t with
  | Var x -> x
  | Abs (x, t) ->
      "\\" ^ x ^ "." ^ term_to_string t
  | App (Var x1, Var x2) -> x1 ^ " " ^ x2
  | App (Var x, t) -> x ^ " (" ^ term_to_string t ^ ")"
  | App (App(t1, t2), Var x) -> "(" ^ term_to_string (App (t1, t2)) ^ ") " ^ x
  | App (t, Var x) -> "(" ^ term_to_string t ^ ")" ^ x
  | App (t1, t2) -> "(" ^ term_to_string t1 ^ ")(" ^ term_to_string t2 ^ ")"

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
  | App (t1, t2) ->
      App (subst t1 x s, subst t2 x s)
  | Abs (y, body) ->
  if x = y then t
  else (
        let free_body = free_vars body in
        let free_s = free_vars s in
        if not (List.mem x free_body) then t
        else (
          if not (List.mem y free_s) then Abs (y, subst body x s)
          else let z = y ^ y in Abs (z, subst (subst body y (Var z)) x s)
        )
      )
