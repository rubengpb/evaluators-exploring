open Core.Syntax
open Core.Utils
open Core.Forms
open Printer


let rec step_bn = function
  | DBApp (DBAbs b, m) ->
      subst_db m 0 b
  | DBApp (m, n) ->
      let m' = step_bn m in
      DBApp (m', n)
  | _ -> failwith "Not redex!"

let rec string_of_term_ss_bn = string_of_dbterm

let rec bn t =
  if is_dbwhnf t then t
  else (
    print_endline @@ string_of_term_ss_bn t;
    t |> step_bn |> bn
  )
