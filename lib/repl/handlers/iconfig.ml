open Evals.Eval
open Config
open Ast
open Core.Utils

let handle_iconfig st = function
    | None -> print_endline @@ string_of_config st; st
    | Some param ->
      (match param with
      | "eval" -> print_endline @@ string_of_eval st.eval
      | "env" -> List.iter
         (fun (v,t) -> print_endline (v ^ " = " ^ string_of_pterm t)) st.env
      | "church" -> print_endline @@ string_of_bool st.church
      | "display" -> print_endline @@ string_of_bool st.display
      | other -> print_endline @@
        "[ERROR in config]\nNon exists this param: " ^ other);
     st
