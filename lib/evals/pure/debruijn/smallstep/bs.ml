open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open Ho


let rec step_bs = function
  | DBApp(m, n) when not (is_dbhnf m) ->
    let m' = step_ho m in
    DBApp(m', n)
  | DBApp(v, n) when (is_dbabs v) && not (is_dbnf n) ->
    let n' = step_ho n in
    DBApp(v, n')
  | DBApp(DBAbs b, v) when (is_dbabs v) ->
    subst_db v 0 b
  | DBApp(v, w) when not (is_dbnf v) ->
    let v' = step_bs v in
    DBApp(v', w)
  | DBApp(v, w) when (is_dbnf v) && not (is_dbnf w)  ->
    let w' = step_bs w in
    DBApp(v, w')
  | DBAbs b when not (is_dbnf b) ->
    let b' = step_bs b in
    DBAbs b'
  | _ -> failwith "Not redex!"

let string_of_term_ss_bs = string_of_dbterm

let rec bs t =
  if is_dbnf t then t
  else (
    print_endline @@ string_of_term_ss_bs t;
    t |> step_bs |> bs
  )
