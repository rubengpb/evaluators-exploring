open Syntax

let rec pterm_of_list lst =
  let c = Var "c" in
  let n = Var "n" in
  let rec build = function
    | [] -> n
    | x :: xs -> App(App (c, x), build xs)
  in
  Abs ("c", Abs ("n", build lst))
