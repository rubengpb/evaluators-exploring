open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open Bn

let rec step_hr t =
  match t with
    | DBApp(m, n) when not (is_dbwhnf m) ->
      let m' = step_bn m in
      DBApp(m', n)
    | DBApp(DBAbs b, n) ->
      subst_db n 0 b
    | DBApp(m, n) when not (is_dbhnf m) ->
      let m' = step_hr m in
      DBApp(m', n)
    | DBAbs b when not (is_dbhnf b) ->
      let b' = step_hr b in
      DBAbs b'
    | _ -> failwith "hr: Not redex!"

let string_of_term_ss_hr = string_of_dbterm

let rec hr t =
  if is_dbhnf t then t
  else (
    print_endline @@ string_of_term_ss_hr t;
    t |> step_hr |> hr
  )
