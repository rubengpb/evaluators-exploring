open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open Ho

let rec step_so t =
  match t with
    | App(m, n) when not (is_hnf m) ->
      let m' = step_ho m in
      App(m', n)
    | App(v, n) when (is_abs v) && not (is_nf n) ->
      let n' = step_so n in
      App(v, n')
    | App(Abs(x, b), v) when is_value v ->
      subst v x b
    | App(m, n) when not (is_nf m) ->
      let m' = step_so m in
      App(m', n)
    | App(v, n) when not (is_nf n) ->
      let n' = step_so n in
      App(v, n')
    | Abs(x, b) when not (is_nf b) ->
      let b' = step_so b in
      Abs(x, b')
    | _ -> failwith "so: Not redex!"

let string_of_term_ss_so = string_of_pterm

let rec so t =
  if is_nf t then t
  else (
    print_endline @@ string_of_term_ss_so t;
    t |> step_so |> so
  )
