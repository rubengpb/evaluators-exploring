open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open Bn

let rec step_hr t =
  match t with
    | App(m, n) when not (is_whnf m) ->
      let m' = step_bn m in
      App(m', n)
    | App(Abs(x, b), n) when is_value n ->
      subst n x b
    | App(m, n) when not (is_hnf m) ->
      let m' = step_hr m in
      App(m', n)
    | Abs(x, b) when not (is_hnf b) ->
      let b' = step_hr b in
      Abs(x, b')
    | _ -> failwith "hr: Not redex!"

let string_of_term_ss_hr = string_of_pterm

let rec hr t =
  if is_hnf t then t
  else (
    print_endline @@ string_of_term_ss_hr t;
    t |> step_hr |> hr
  )
