open Core.Syntax
open Core.Utils

let color_code color =
  match String.lowercase_ascii color with
  | "black" -> "30"
  | "red" -> "31"
  | "green" -> "32"
  | "yellow" -> "33"
  | "blue" -> "34"
  | "magenta" -> "35"
  | "cyan" -> "36"
  | "white" -> "37"
  | _ -> "37"

let colorize text color =
  let code = color_code color in
  Printf.sprintf "\027[1;4;%sm%s\027[0m" code text

let rec string_of_term_ss_outer = function
  | Var x -> x
  | Abs (x, t) ->
      "\\" ^ x ^ "." ^ string_of_term_ss_outer t 
  | App (Var x1, Var x2) -> x1 ^ " " ^ x2
  | App (Var x, t) -> x ^ " (" ^ string_of_term_ss_outer t  ^ ")"
  | App (App(t1, t2), Var x) -> string_of_term_ss_outer (App (t1, t2))  ^ " " ^ x
  | App (App(t1, t2), Abs(x,t)) -> string_of_term_ss_outer (App (t1, t2))  ^ " (" ^ string_of_term_ss_outer (Abs(x,t))  ^ ")"
  | App (App(t1, t2), t) -> string_of_term_ss_outer (App (t1, t2)) ^ " (" ^ string_of_term_ss_outer t ^ ")"
  | App (Abs (x, body) as l, Var y) ->
      let left = colorize (string_of_term l) "blue" in
      let right = colorize y "red" in
      "(" ^ left ^ ") " ^ right
  | App (Abs (x, body) as l, arg) ->
      let left = colorize (string_of_term l) "blue" in
      let right = colorize (string_of_term arg) "red" in
      "(" ^ left ^ ") (" ^ right ^ ")"

let rec string_of_term_ss_inner = function
  | Var x -> x
  | Abs (x, t) ->
      "\\" ^ x ^ "." ^ string_of_term_ss_inner t
  | App (Var x1, Var x2) -> x1 ^ " " ^ x2
  | App (Var x, t) -> x ^ " (" ^ string_of_term_ss_inner t  ^ ")"
  | App (App(t1, t2), Var x) -> string_of_term_ss_inner (App (t1, t2))  ^ " " ^ x
  | App (App(t1, t2), Abs(x,t)) -> string_of_term_ss_inner (App (t1, t2))  ^ " (" ^ string_of_term_ss_inner (Abs(x,t))  ^ ")"
  | App (App(t1, t2), t) -> string_of_term_ss_inner (App (t1, t2)) ^ " (" ^ string_of_term_ss_inner t ^ ")"
  | App (Abs (x, body) as l, Var y) ->
      if is_there_redex body then
        "(" ^ string_of_term_ss_inner l ^ ")" ^ y
      else
        let left = colorize (string_of_term l) "blue" in
        let right = colorize y "red" in
        "(" ^ left ^ ") " ^ right
  | App (Abs (x, body) as l, arg) ->
      if is_there_redex body then
        "(" ^ string_of_term_ss_inner l ^ ") (" ^ string_of_term_ss_inner arg ^ ")"
      else
        let left = colorize (string_of_term l) "blue" in
        let right = colorize (string_of_term arg) "red" in
        "(" ^ left ^ ") (" ^ right ^ ")"

let string_of_term_ss t = function
  | "outermost" -> string_of_term_ss_outer t
  | "innermost" -> string_of_term_ss_inner t
  | _ -> string_of_term t

let print_ss t opt =
  Printf.printf "%s\n" (string_of_term_ss t opt)
