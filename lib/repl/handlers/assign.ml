open Ast
open Config
open Envm
open Core.Utils

let handle_assing st v t =
  let t' = expand st.env t in
  print_endline ("Assigned " ^ v ^ " = " ^ string_of_pterm t');
  { st with env = (v,t',t) :: st.env }
