open Ast
open Config
open Envm

let handle_assing st v t =
  let t' = expand st.env t in
  print_endline ("Assigned " ^ v ^ " = " ^ show_term t');
  { st with env = (v,t') :: st.env }
