open Core.Syntax

let is_value = function
  | App(_,_) -> false
  | _ -> true

let is_dbvalue = function
  | DBApp(_,_) -> false
  | _ -> true

let is_cvalue = function
  | CApp _ -> false
  | Clou(CApp _, _) -> false
  | _ -> true
