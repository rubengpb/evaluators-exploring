open Repl

let initial_state = {
  eval = Normal;
  env = [];
}

let () =
  print_endline @@ "λ-REPL evaluators-exploring, version 0.1.0:  " ^
    ":h for help,  :q for exit";
  loop initial_state
