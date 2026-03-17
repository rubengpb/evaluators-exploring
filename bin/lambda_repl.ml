open Repl.Main_repl
open Repl.Config
open Evals.Eval

let initial_config = {
  eval = One { language = Pure; style = Apply; strategy = NormalOrder; };
  env = [];
  church = false;
  display = false;
}

let () =
  Sys.catch_break true;
  print_endline @@ "λ-REPL evaluators-exploring, version 0.3.0:  " ^
    ":h for help,  :q for exit";
  loop initial_config
