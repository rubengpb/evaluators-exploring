open Repl.Main_repl
open Repl.Config

let initial_config = {
  eval = Normal;
  env = [];
}

let () =
  print_endline @@ "λ-REPL evaluators-exploring, version 0.1.1:  " ^
    ":h for help,  :q for exit";
  loop initial_config
