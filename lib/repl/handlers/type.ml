open Ast
open Envm
open Config
open Core.Hm

let handle_type st id =
  match List.assoc_opt id st.env with
  | Some t ->
    print_endline @@ id ^ " :: " ^ string_of_type (fst @@ infer [] t); st
  | None ->
    print_endline "Identifier not initialized."; st
