open Core.Syntax

let is_value = function
  | App(_,_) -> false
  | _ -> true

let is_dbvalue = function
  | DBApp(_,_) -> false
  | _ -> true
