open Evals.Eval
open Config

let handle_set st s =
  match eval_of_string s with
   | Some ev ->
        print_endline ("Evaluator set to " ^ s);
        { st with eval = ev }
   | None ->
        print_endline ("Unknown evaluator: " ^ s);
        st
