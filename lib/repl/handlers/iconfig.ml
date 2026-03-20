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
        (fun (v,_,t) -> print_endline (v ^ " = " ^ string_of_pterm t)) st.env
      | "church" -> print_endline @@ string_of_bool st.church
      | "display" -> print_endline @@ string_of_bool st.display
      | other -> (
        match String.split_on_char '_' other with
        | "env":: vars -> let var = String.concat "_" vars in
            (match List.assoc_opt var (List.map (fun (v,_,t) -> (v,t)) st.env) with
             | Some t -> print_endline @@ (var ^ " = " ^ string_of_pterm t)
             | None -> print_endline @@ "[ERROR in config]\nUndefined variable: " ^ var)
        | _ -> print_endline @@ "[ERROR in config]\nUndefined parameter for config: " ^ other)
      );
     st
