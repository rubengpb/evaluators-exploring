open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open Ho

let rec step_so t =
  match t with
    | DBApp(m, n) when not (is_dbhnf m) ->
      let m' = step_ho m in
      DBApp(m', n)
    | DBApp(v, n) when (is_dbabs v) && not (is_dbnf n) ->
      let n' = step_so n in
      DBApp(v, n')
    | DBApp(DBAbs b, v) ->
      subst_db v 0 b
    | DBApp(m, n) when not (is_dbnf m) ->
      let m' = step_so m in
      DBApp(m', n)
    | DBApp(v, n) when not (is_dbnf n) ->
      let n' = step_so n in
      DBApp(v, n')
    | DBAbs b when not (is_dbnf b) ->
      let b' = step_so b in
      DBAbs b'
    | _ -> failwith "so: Not redex!"

let string_of_term_ss_so = string_of_dbterm

let rec so t =
  if is_dbnf t then t
  else (
    print_endline @@ string_of_term_ss_so t;
    t |> step_so |> so
  )
