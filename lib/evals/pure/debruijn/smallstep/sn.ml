open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open Bv

let rec step_sn t =
  match t with
    | DBApp(m, n) when not (is_dbwnf m) ->
      let m' = step_bv m in
      DBApp(m', n)
    | DBApp(v, n) when (is_dbabs v) && not (is_dbwnf n) ->
      let n' = step_bv n in
      DBApp(v, n')
    | DBApp(DBAbs b, v) ->
      subst_db v 0 b
    | DBApp(m, n) when not (is_dbnf m) ->
      let m' = step_sn m in
      DBApp(m', n)
    | DBApp(v, n) when not (is_dbnf n) ->
      let n' = step_sn n in
      DBApp(v, n')
    | DBAbs b when not (is_dbnf b) ->
      let b' = step_sn b in
      DBAbs b'
    | _ -> failwith "sn: Not redex!"

let string_of_term_ss_sn = string_of_dbterm

let rec sn t =
  if is_dbnf t then t
  else (
    print_endline @@ string_of_term_ss_sn t;
    t |> step_sn |> sn
  )
