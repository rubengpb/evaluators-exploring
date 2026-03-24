open Core.Syntax
open Core.Utils

let color_code color =
  match String.lowercase_ascii color with
  | "black" -> "30"
  | "red" -> "31"
  | "green" -> "32"
  | "yellow" -> "33"
  | "blue" -> "34"
  | "magenta" -> "35"
  | "cyan" -> "36"
  | "white" -> "37"
  | _ -> "37"

let colorize text color =
  let code = color_code color in
  Printf.sprintf "\027[1;4;%sm%s\027[0m" code text
