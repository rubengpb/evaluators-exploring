open Core.Syntax
open Core.Utils
open Core.Forms
open Printer

let rec step_he t =
  match t with
    | DBApp(m, n) when not (is_dbhnf m) ->
      let m' = step_he m in
      DBApp(m', n)
    | DBApp(DBAbs b, n) when is_dbvalue n ->
      subst_db n 0 b
    | DBAbs b when not (is_dbhnf b) ->
      let b' = step_he b in
      DBAbs b'
    | _ -> failwith "he: Not redex!"

let string_of_term_ss_he = string_of_dbterm

let rec he t =
  if is_dbhnf t then t
  else (
    print_endline @@ string_of_term_ss_he t;
    t |> step_he |> he
  )
