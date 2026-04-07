open Config
open Ast
open Envm
open Main_parse
open Assign

let rec load_file st filename =
  try
    let ic = open_in filename in

    let rec loop st =
      match input_line ic with
      | line ->
          let words = String.split_on_char ' ' line in
          let st =
            match words with
            | "include" :: files ->
                List.fold_left load_file st files

            | _ ->
                match parse line with
                | Assign (v, t) ->
                    let t' = expand st.env t in
                    { st with env = add_to_main_env st.env v t' t }

                | _ ->
                    failwith ("Invalid line in load file: " ^ line)
          in
          loop st

      | exception End_of_file ->
          close_in ic;
          st
    in
    loop st

  with
  | Sys_error msg ->
      failwith ("File error. " ^ msg)
  | Failure msg ->
      failwith ("File error. " ^ msg)

let handle_file st file =
  print_endline "Reading file...";
  load_file st file
