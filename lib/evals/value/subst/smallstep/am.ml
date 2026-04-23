open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open Bv


let rec step_am = function
  | App(m, n) when not (is_wnf m) ->
    let m' = step_bv m in
    App(m', n)
  | App(v, n) when (is_abs v) && not (is_wnf n) ->
    let n' = step_bv n in
    App(v, n')
  | App(Abs(x, b), v) when is_value v ->
    subst v x b
  | Abs(x, b) when not (is_vhnf b)->
    let b' = step_am b in
    Abs(x, b')
  | _ -> failwith "Not redex!"

let string_of_term_ss_am = string_of_pterm

let rec am t =
  if is_vhnf t then t
  else (
    print_endline @@ string_of_term_ss_am t;
    t |> step_am |> am
  )
