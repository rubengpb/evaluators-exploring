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

let rec string_of_pterm_with_options num list t =
  match num, list with
    | true, true -> (
      match int_of_pterm t with
        | Some n -> string_of_int n
        | _ -> (match list_of_pterm t with
                | Some xs -> string_of_pterm_list true xs
                | _ -> string_of_pterm t)
      )
    | true, false -> (
      match int_of_pterm t with
        | Some n -> string_of_int n
        | _ -> string_of_pterm t
    )
    | false, true -> (
      match list_of_pterm t with
        | Some xs -> string_of_pterm_list false xs
        | _ -> string_of_pterm t
    )
    | _ -> string_of_pterm t
and string_of_pterm_list num xs =
  "[" ^ List.fold_left (fun acc s -> acc ^ string_of_pterm_with_options num true s ^ "; ") "" xs ^ "]"

let string_of_dbterm_with_options num t =
  if num then
    match int_of_dbterm t with
      | Some n -> string_of_int n
      | None -> string_of_dbterm_pure t
  else string_of_dbterm_pure t

let string_of_cterm_with_options num t =
  if num then
    match int_of_cterm t with
      | Some n -> string_of_int n
      | None -> string_of_cterm t
  else string_of_cterm t

let string_of_term_with_options num list = function
  | TPure tt -> string_of_pterm_with_options num list tt
  | TDeBruijn dbt -> string_of_dbterm_with_options num dbt
  | TClousure clt -> string_of_cterm_with_options num clt

let handle_term st t =
  let t = expand st.env t in
  let t = (match st.simulator with
            | None -> t
            | Some sim -> simulation_transform sim t) in
  if st.display then print_endline @@ "Evaluating: " ^ string_of_pterm t;
  let t = eval st.eval t in
    print_endline @@ string_of_term_with_options st.church_num st.church_list t;
  st
