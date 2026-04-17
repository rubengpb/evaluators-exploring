open Core.Syntax
open Core.Utils
open Core.Forms
open Printer

let rec step_ao = function
  | DBApp(m, n) when not (is_dbnf m) ->
    let m' = step_ao m in
    DBApp(m', n)
  | DBApp(v, n) when not (is_dbnf n) ->
    let n' = step_ao n in
    DBApp(v, n')
  | DBApp(DBAbs b, v) ->
    subst_db v 0 b
  | DBAbs b when not (is_dbnf b)->
    let b' = step_ao b in
    DBAbs b'
  | _ -> failwith "Not redex!"

let rec string_of_term_ss_ao = string_of_dbterm

let rec ao t =
  if is_dbnf t then t
  else (
    print_endline @@ string_of_term_ss_ao t;
    t |> step_ao |> ao
  )
