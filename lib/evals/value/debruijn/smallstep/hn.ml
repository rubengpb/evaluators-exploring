open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open He

let rec step_hn t =
  match t with
    | DBApp(m, n) when not (is_dbhnf m) ->
      let m' = step_he m in
      DBApp(m', n)
    | DBApp(DBAbs b, n) when is_dbvalue n ->
      subst_db n 0 b
    | DBApp(m, n) when not (is_dbnf m) ->
      let m' = step_hn m in
      DBApp(m', n)
    | DBApp(v, n) when not (is_dbnf n) ->
      let n' = step_hn n in
      DBApp(v, n')
    | DBAbs b when not (is_dbnf b) ->
      let b' = step_hn b in
      DBAbs b'
    | _ -> failwith "hn: Not redex!"

let string_of_term_ss_hn = string_of_dbterm

let rec hn t =
  if is_dbnf t then t
  else (
    print_endline @@ string_of_term_ss_hn t;
    t |> step_hn |> hn
  )
