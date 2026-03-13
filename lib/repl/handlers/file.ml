open Config
open Ast
open Envm
open Main_parse

let load_file filename =
  try
  (let ic = open_in filename in
  let rec loop_assing acc =
    match input_line ic with
    | line ->
        let acc =
          match parse line with
          | Assign (v, t) -> (v, t) :: acc
          | _ ->
              failwith ("Invalid line in load file: " ^ line)
        in
        loop_assing acc
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in
  loop_assing [])
  with
     | Sys_error msg ->
         print_endline ("File error: " ^ msg);
        []
     | Failure msg ->
         print_endline msg;
        []

let handle_file st file =
  let assigns = load_file file in
  let st' =
    List.fold_left
      (fun st (v,t) ->
         let t' = expand st.env t in
         { st with env = (v,t') :: st.env })
      st
      assigns
  in
    print_endline "Reading file...";
    st'
