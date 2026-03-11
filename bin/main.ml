open Core.Syntax
open Core.Strategy
open Core.Utils
open Evals.Main_eval

let main_term = term_of_string "\\x.x"


let () =
  let t_no = eval Normal main_term in
    print_endline @@ string_of_term main_term;
