open Evals.Eval
open Config

let handle_set st param opt =
  match param with
  | "env" -> { st with env = [] }
  | "eval" ->
    (match eval_of_string opt with
    | Some ev -> { st with eval = ev }
    | None ->
        print_endline @@ "[ERROR in set eval]\nNon exists this eval";
        st)
  | "church" ->
    if opt = "true" then { st with church = true }
    else { st with church = false }
  | "display" ->
    if opt = "true" then { st with display = true }
    else { st with display = false }
  | other ->
    print_endline @@ "[ERROR in set]\nNon exists this param: " ^ other;
    st
