open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open Bv

let rec step_ha t =
  match t with
    | App(m, n) when not (is_wnf m) ->
      let m' = step_bv m in
      App(m', n)
    | App(v, n) when (is_abs v) && not (is_nf n) ->
      let n' = step_ha n in
      App(v, n')
    | App(Abs(x, b), v) when is_value v ->
      subst v x b
    | App(m, n) when not (is_nf m) ->
      let m' = step_ha m in
      App(m', n)
    | App(v, n) when not (is_nf n) ->
      let n' = step_ha n in
      App(v, n')
    | Abs(x, b) when not (is_nf b) ->
      let b' = step_ha b in
      Abs(x, b')
    | _ -> failwith "ha: Not redex!"

let string_of_term_ss_ha = string_of_pterm

let rec ha t =
  if is_nf t then t
  else (
    print_endline @@ string_of_term_ss_ha t;
    t |> step_ha |> ha
  )
