open Core.Syntax
open Core.Utils
open Core.Forms
open Printer
open Bv


let rec step_am = function
  | DBApp(m, n) when not (is_dbwnf m) ->
    let m' = step_bv m in
    DBApp(m', n)
  | DBApp(v, n) when (is_dbabs v) && not (is_dbwnf n) ->
    let n' = step_bv n in
    DBApp(v, n')
  | DBApp(DBAbs b, v) when is_dbvalue v ->
    subst_db v 0 b
  | DBAbs b when not (is_dbvhnf b)->
    let b' = step_am b in
    DBAbs b'
  | _ -> failwith "Not redex!"

let string_of_term_ss_am = string_of_dbterm

let rec am t =
  if is_dbvhnf t then t
  else (
    print_endline @@ string_of_term_ss_am t;
    t |> step_am |> am
  )
