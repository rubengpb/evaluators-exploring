open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open Ho


let rec step_bs = function
  | App(m, n) when not (is_hnf m) ->
    let m' = step_ho m in
    App(m', n)
  | App(v, n) when (is_abs v) && not (is_hnf n) ->
    let n' = step_ho n in
    App(v, n')
  | App(Abs(x, b), v) ->
    subst v x b
  | App(v, w) when not (is_nf v) ->
    let v' = step_bs v in
    App(v', w)
  | App(v, w) when (is_nf v) && not (is_nf w)  ->
    let w' = step_bs w in
    App(v, w')
  | Abs(x, b) when not (is_nf b) ->
    let b' = step_bs b in
    Abs(x, b')
  | _ -> failwith "Not redex!"

let string_of_term_ss_bs = string_of_pterm

let rec bs t =
  if is_nf t then t
  else (
    print_endline @@ string_of_term_ss_bs t;
    t |> step_bs |> bs
  )
