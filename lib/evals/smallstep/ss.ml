open Core.Utils
open Printer

let rec eval_ss is_value opt step t =
  if is_value t then t
  else (
    print_ss t opt;
    t |> step |> (eval_ss is_value opt step)
  )
