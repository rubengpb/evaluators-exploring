open Evals.Eval
open Config
open Ast
open Core.Utils

let handle_iconfig st = function
    | None -> print_endline @@ string_of_config st; st
    | Some param ->
      (match param with
      | "lan" | "language" -> print_endline @@ string_of_language st.eval.language
      | "sub" | "subst" -> print_endline @@ string_of_subst st.eval.subst
      | "sty" | "style" -> print_endline @@ string_of_style st.eval.style
      | "str" | "strategy" -> print_endline @@ string_of_strategy st.eval.strategy
      | "sim" | "simulator" -> print_endline @@ string_of_simulator_option st.simulator
      | "eval" -> print_endline @@ string_of_eval st.eval
      | "env" -> List.iter
        (fun (v,_,t) -> print_endline (v ^ " = " ^ string_of_pterm t)) st.env
      | "expEnv" | "expandedEnv" -> List.iter
        (fun (v,t,_) -> print_endline (v ^ " = " ^ string_of_pterm t)) st.env
      | "church" -> print_endline @@ string_of_church st.church_num st.church_list
      | "church_num" | "ch_num" -> print_endline @@ string_of_onoff st.church_num
      | "church_list" | "ch_list" -> print_endline @@ string_of_onoff st.church_list
      | "display" -> print_endline @@ string_of_onoff st.display
      | other -> (
        match String.split_on_char '_' other with
        | "env":: vars -> let var = String.concat "_" vars in
            (match List.assoc_opt var (List.map (fun (v,_,t) -> (v,t)) st.env) with
             | Some t -> print_endline @@ (var ^ " = " ^ string_of_pterm t)
             | None -> print_endline @@ "[ERROR in config]\nUndefined variable: " ^ var)
        | "expEnv"::vars | "expandedEnv":: vars ->
          let var = String.concat "_" vars in
            (match List.assoc_opt var (List.map (fun (v,t,_) -> (v,t)) st.env) with
             | Some t -> print_endline @@ (var ^ " = " ^ string_of_pterm t)
             | None -> print_endline @@ "[ERROR in config]\nUndefined variable: " ^ var)
        | _ -> print_endline @@ "[ERROR in config]\nUndefined parameter for config: " ^ other)
      );
     st
