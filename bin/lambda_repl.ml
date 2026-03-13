open Repl.Main_repl
open Repl.Config

let initial_config = {
  eval = Normal;
  env = [];
  church = false;
  display = false;
}

let () =
  print_endline @@ "λ-REPL evaluators-exploring, version 0.2.0:  " ^
    ":h for help,  :q for exit";
  loop initial_config
