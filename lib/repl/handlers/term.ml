open Ast
open Envm
open Config
open Evals.Main_eval
open Evals.Eval
open Core.Utils
open Core.Church_numerals

let handle_term st t =
  let t = expand st.env t in
  if st.display then print_endline @@ "Evaluating: " ^ string_of_pterm t;
  let t = eval st.eval t in (
    if st.church then
      match int_of_term t with
        | Some n -> print_endline @@ string_of_int n
        | None -> print_endline @@ string_of_term t
    else print_endline @@ string_of_term t);
  st
