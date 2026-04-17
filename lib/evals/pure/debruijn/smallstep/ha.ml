open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open Bv

let rec step_ha t =
  match t with
    | DBApp(m, n) when not (is_dbwnf m) ->
      let m' = step_bv m in
      DBApp(m', n)
    | DBApp(v, n) when (is_dbabs v) && not (is_dbnf n) ->
      let n' = step_ha n in
      DBApp(v, n')
    | DBApp(DBAbs b, v) ->
      subst_db v 0 b
    | DBApp(m, n) when not (is_dbnf m) ->
      let m' = step_ha m in
      DBApp(m', n)
    | DBApp(v, n) when not (is_dbnf n) ->
      let n' = step_ha n in
      DBApp(v, n')
    | DBAbs b when not (is_dbnf b) ->
      let b' = step_ha b in
      DBAbs b'
    | _ -> failwith "ha: Not redex!"

let string_of_term_ss_ha = string_of_dbterm

let rec ha t =
  if is_dbnf t then t
  else (
    print_endline @@ string_of_term_ss_ha t;
    t |> step_ha |> ha
  )
