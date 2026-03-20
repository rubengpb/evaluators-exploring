open Config
open Ast
open Envm
open Main_parse
open Assign

let rec load_file filename =
  try
    let ic = open_in filename in
    let first_line =
      try Some (input_line ic) with End_of_file -> None
    in
    let initial_acc =
      match first_line with
      | Some line ->
          let words = String.split_on_char ' ' line in
          (match words with
           | "include" :: files ->
               List.flatten (List.map load_file files)
           | _ ->
               match parse line with
               | Assign (v,t) -> [(v,t)]
               | _ -> failwith ("Invalid line in load file: " ^ line))
      | None -> []
    in
    let rec loop_assign acc =
      match input_line ic with
      | line ->
          let acc =
            match parse line with
            | Assign (v,t) -> (v,t) :: acc
            | _ -> failwith ("Invalid line in load file: " ^ line)
          in
          loop_assign acc
      | exception End_of_file ->
          close_in ic;
          List.rev acc
    in
    loop_assign initial_acc
  with
  | Sys_error msg ->
      failwith ("File error. " ^ msg)
  | Failure msg ->
      failwith ("File error. " ^ msg)

let handle_file st file =
  let assigns = load_file file in
  let st' =
    List.fold_left
      (fun st (v,t) ->
         let t' = expand st.env t in
         { st with env = add_to_main_env st.env v t' t})
      st
      assigns
  in
    print_endline "Reading file...";
    st'
