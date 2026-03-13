open Core.Utils

let rec eval_ss is_value step t =
  if is_value t then t
  else (
    print_endline (string_of_term t);
    t |> step |> (eval_ss is_value step)
  )
