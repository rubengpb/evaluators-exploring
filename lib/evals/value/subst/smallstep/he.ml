open Core.Syntax
open Core.Utils
open Core.Forms
open Printer

let rec step_he t =
  match t with
    | App(m, n) when not (is_hnf m) ->
      let m' = step_he m in
      App(m', n)
    | App(Abs(x, b), n) when is_value n ->
      subst n x b
    | Abs(x, b) when not (is_hnf b) ->
      let b' = step_he b in
      Abs(x, b')
    | _ -> failwith "he: Not redex!"

let string_of_term_ss_he = string_of_pterm

let rec he t =
  if is_hnf t then t
  else (
    print_endline @@ string_of_term_ss_he t;
    t |> step_he |> he
  )
