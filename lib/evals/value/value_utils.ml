open Core.Syntax

let is_value = function
  | Abs(_,_) -> true
  | _ -> false
