open Core.Syntax
open Core.Utils
open Core.Forms
open Printer

let rec step_bv = function
  | DBApp(m, n) when not (is_dbwnf m) ->
    let m' = step_bv m in
    DBApp(m', n)
  | DBApp(v, n) when not (is_dbwnf n) ->
    let n' = step_bv n in
    DBApp(v, n')
  | DBApp (DBAbs b, v) ->
    subst_db v 0 b
  | _ -> failwith "Not redex!"

let rec string_of_term_ss_bv = string_of_dbterm

let rec bv t =
  if is_dbwnf t then t
  else (
    print_endline @@ string_of_term_ss_bv t;
    t |> step_bv |> bv
  )
