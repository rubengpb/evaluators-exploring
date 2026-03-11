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
    List.sort_uniq String.compare @@ free_vars t1 @ free_vars t2

let rec new_free_var x xs =
  let y = x ^ "s" in
  if List.mem y xs then new_free_var y xs
  else y

let rec subst n x b =
  match b with
    | Var y ->
        if y = x then n else b
    | App (t1, t2) ->
        App (subst n x t1, subst n x t2)
    | Abs (y, body) ->
      if x = y then b
      else (
          let free_body = free_vars body in
          let free_n = free_vars n in
          if not (List.mem x free_body) then b
          else if not (List.mem y free_n) then Abs (y, subst n x body)
          else let z = new_free_var y free_body in
            Abs (z, subst n x (subst (Var z) y body))
        )

let rec alpha_equiv t1 t2 =
  match (t1, t2) with
    | (Var x, Var y) -> x = y
    | (Abs(x, b1), Abs(y, b2)) ->
      if x = y then alpha_equiv b1 b2
      else alpha_equiv b1 @@ subst (Var x) y b2
    | (App(t1, t2), App(tt1, tt2)) -> alpha_equiv t1 tt1 && alpha_equiv t2 tt2
    | _ -> false
