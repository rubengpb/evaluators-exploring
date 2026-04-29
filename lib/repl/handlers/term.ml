open Ast
open Envm
open Config
open Evals.Main_eval
open Evals.Eval
open Evals.Simulator
open Core.Utils
open Core.Church_numerals
open Core.Church_list
open Core.Syntax

let rec string_of_term_with_options num list t =
  match num, list with
    | true, true -> (
      match int_of_term t with
        | Some n -> string_of_int n
        | _ -> (match list_of_term t with
        | Some xs -> "[" ^ string_of_term_list true xs ^ "]"
                | _ -> string_of_term t)
      )
    | true, false -> (
      match int_of_term t with
        | Some n -> string_of_int n
        | _ -> string_of_term t
    )
    | false, true -> (
      match list_of_term t with
        | Some xs -> "[" ^ string_of_term_list false xs ^ "]"
        | _ -> string_of_term t
    )
    | _ -> string_of_term t
and string_of_term_list num xs =
  match xs with
    | [] -> ""
    | [x] -> string_of_term_with_options num true x
    | x::xs ->
      string_of_term_with_options num true x ^ "; " ^ string_of_term_list num xs

let handle_term st t =
  let t = expand st.env t in
  let t = (match st.simulator with
            | None -> t
            | Some sim -> simulation_transform sim t) in
  if st.display then print_endline @@ "Evaluating: " ^ string_of_pterm t;
  let t = eval st.eval t in
    print_endline @@ string_of_term_with_options st.church_num st.church_list t;
    (match st.simulator with
      | None -> ()
      | Some sim ->
        print_endline @@
          string_of_term_with_options
            st.church_num
            st.church_list
            (inverse_simulation_transform sim st.eval t)
    );
  st
