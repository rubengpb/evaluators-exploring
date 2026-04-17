open Core.Syntax
open Core.Utils
open Core.Forms
open Printer


let rec step_ho = function
  | DBApp(m, n) when not (is_dbhnf m) ->
    let m' = step_ho m in
    DBApp(m', n)
  | DBApp(v, n) when (is_dbabs v) && not (is_dbhnf n) ->
    let n' = step_ho n in
    DBApp(v, n')
  | DBApp(DBAbs b, v) ->
    subst_db v 0 b
  | DBAbs b when not (is_dbhnf b) ->
    let b' = step_ho b in
    DBAbs b'
  | _ -> failwith "Not redex!"

let string_of_term_ss_ho = string_of_dbterm

let rec ho t =
  if is_dbhnf t then t
  else (
    print_endline @@ string_of_term_ss_ho t;
    t |> step_ho |> ho
  )
