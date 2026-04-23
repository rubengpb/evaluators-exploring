open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open Bn


let rec step_no = function
  | DBApp(m, n) when not (is_dbwhnf m) ->
    let m' = step_bn m in
    DBApp(m', n)
  | DBApp(DBAbs b, n) when is_dbvalue n ->
    subst_db n 0 b
  | DBApp(m, n) when not (is_dbnf m) ->
    let m' = step_no m in
    DBApp(m', n)
  | DBApp(v, n) when not (is_dbnf n) ->
    let n' = step_no n in
    DBApp(v, n')
  | DBAbs b when not (is_dbnf b) ->
    let b' = step_no b in
    DBAbs b'
  | _ -> failwith "Not redex!"

let rec string_of_term_ss_no = string_of_dbterm

let rec no t =
  if is_dbnf t then t
  else (
    print_endline @@ string_of_term_ss_no t;
    t |> step_no |> no
  )
