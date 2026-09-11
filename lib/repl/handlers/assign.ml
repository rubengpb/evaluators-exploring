open Ast
open Config
open Envm
open Core.Utils

let add_to_main_env env v t' t =
  let cleaned_env = List.filter (fun (x,_,_) -> x <> v) env in
  (v,t',t) :: cleaned_env

let handle_assign st v t =
  let t' = expand st.env t in
  print_endline ("Assigned " ^ v ^ " = " ^ string_of_pterm t');
  { st with env = add_to_main_env st.env v t' t}
