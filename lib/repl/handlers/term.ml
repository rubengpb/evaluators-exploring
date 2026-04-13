open Ast
open Envm
open Config
open Evals.Main_eval
open Evals.Eval
open Evals.Simulator
open Core.Utils
open Core.Church_numerals
open Core.Church_list

let handle_option_display num list t =
  if num then
    match int_of_term t with
      | Some n -> print_endline @@ string_of_int n
      | _ -> if list then
          match list_of_term t with
              | Some xs -> print_endline @@ string_of_pterm_list xs
              | _ -> print_endline @@ string_of_term t
          else
            print_endline @@ string_of_term t
  else if list then
    match list_of_term t with
      | Some xs -> print_endline @@ string_of_pterm_list xs
      | _ -> print_endline @@ string_of_term t
  else
    print_endline @@ string_of_term t


let handle_term st t =
  let t = expand st.env t in
  let t = (match st.simulator with
            | None -> t
            | Some sim -> simulation_transform sim t) in
  if st.display then print_endline @@ "Evaluating: " ^ string_of_pterm t;
  let t = eval st.eval t in
    handle_option_display st.church_num st.church_list t;
  st
