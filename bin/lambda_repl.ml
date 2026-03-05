open Repl

let initial_state = {
  eval = Normal;
  env = [];
}

let () =
  loop initial_state
