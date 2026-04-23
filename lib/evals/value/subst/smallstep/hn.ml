open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open He

let rec step_hn t =
  match t with
    | App(m, n) when not (is_hnf m) ->
      let m' = step_he m in
      App(m', n)
    | App(Abs(x, b), n) when is_value n ->
      subst n x b
    | App(m, n) when not (is_nf m) ->
      let m' = step_hn m in
      App(m', n)
    | App(v, n) when not (is_nf n) ->
      let n' = step_hn n in
      App(v, n')
    | Abs(x, b) when not (is_nf b) ->
      let b' = step_hn b in
      Abs(x, b')
    | _ -> failwith "hn: Not redex!"

let string_of_term_ss_hn = string_of_pterm

let rec hn t =
  if is_nf t then t
  else (
    print_endline @@ string_of_term_ss_hn t;
    t |> step_hn |> hn
  )
