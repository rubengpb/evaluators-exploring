open Main_parse
open Config
open Ast

let handle_command st = function
  | Instr i ->
    (match i with
     | Quit -> print_endline "Bye!"; exit 0
     | Info -> Info.handle_info st
     | Help -> Help.handle_help st
     | Envm -> Envm.handle_envm st
     | Set s -> Set.handle_set st s
     | Type x -> print_endline "Type not implemented yet"; st
     | Load file -> File.handle_file st file)
  | Assign (v, t) -> Assign.handle_assing st v t
  | Term t -> Term.handle_term st t

let prompt = "λ> "

let rec loop st =
  print_string prompt;
  flush stdout;

  match read_line () with
  | exception End_of_file -> print_endline "\nBye!"
  | line ->
      let st' =
        try
          let ast = parse line in
          handle_command st ast
        with
        | Failure msg ->
            print_endline ("Failure: " ^ msg);
            st
        | Parser.Error ->
            print_endline "Parse error";
            st
      in
      loop st'
