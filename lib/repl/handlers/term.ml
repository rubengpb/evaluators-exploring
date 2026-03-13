open Ast
open Envm
open Config
open Evals.Main_eval
open Core.Church_numerals

let handle_term st t =
  let t' = expand st.env t in
  print_endline @@ show_term t';
  print_endline "Evaluating...";
  let t'' = eval st.eval t' in
  (match int_of_term t'' with
    | Some n -> print_endline @@ string_of_int n
    | None -> print_endline @@ show_term t'');
  st
