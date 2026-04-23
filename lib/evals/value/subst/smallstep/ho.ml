open Core.Syntax
open Core.Utils
open Core.Forms
open Printer


let rec step_ho = function
  | App(m, n) when not (is_hnf m) ->
    let m' = step_ho m in
    App(m', n)
  | App(v, n) when (is_abs v) && not (is_hnf n) ->
    let n' = step_ho n in
    App(v, n')
  | App(Abs(x, b), v) when is_value v ->
    subst v x b
  | Abs(x, b) when not (is_hnf b) ->
    let b' = step_ho b in
    Abs(x, b')
  | _ -> failwith "Not redex!"

let string_of_term_ss_ho = string_of_pterm

let rec ho t =
  if is_hnf t then t
  else (
    print_endline @@ string_of_term_ss_ho t;
    t |> step_ho |> ho
  )
