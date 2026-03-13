open Repl.Main_repl
open Repl.Config
open Evals.Eval

let initial_config = {
  eval = One { style = Apply; strategy = NormalOrder; };
  env = [];
  church = false;
  display = false;
}

let () =
  print_endline @@ "λ-REPL evaluators-exploring, version 0.2.0:  " ^
    ":h for help,  :q for exit";
  loop initial_config
