open Parser
open Lexer
open Ast
open Core.Syntax
open Core.Strategy
open Core.Utils
open Core.Church_numerals
open Evals.Main_eval

type state = {
  eval : strategy;
  env  : (string * term) list;
}

let token_to_string = function
  | Q -> "Q"
  | INFO -> "INFO"
  | SET -> "SET"
  | TYPE -> "TYPE"
  | H -> "H"
  | ENVM -> "ENVM"
  | LOAD -> "LOAD"
  | EQUAL -> "EQUAL"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LAMBDA -> "LAMBDA"
  | DOT -> "DOT"
  | IDENT s -> "IDENT(" ^ s ^ ")"
  | NUMBER n -> "NUMBER(" ^ (string_of_int n) ^ ")"
  | EOF -> "EOF"

let rec print_tokens lexbuf =
  let tok = read lexbuf in
  print_endline (token_to_string tok);
  if tok <> EOF then print_tokens lexbuf

let parse line =
  let lexbuf = Lexing.from_string line in
  repl read lexbuf

let rec expand env = function
  | Var x ->
      (match List.assoc_opt x env with
       | Some t -> t
       | None -> Var x)

  | Abs (x,t) ->
      Abs (x, expand (List.remove_assoc x env) t)

  | App (t1,t2) ->
      App (expand env t1, expand env t2)


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

let handle_command st = function
  | Instr i ->
      (match i with
       | Quit ->
           print_endline "Bye!";
           exit 0

       | Info ->
           print_endline @@ strategy_to_string st.eval;
           st

       | Help ->
           print_endline
             ("Simple λ-REPL: type lambda-terms, assign lambda-terms" ^
             " and use them. \n\nType :q for exit." ^
              "\nType :h for help." ^
              "\nType :info to know the current strategy." ^
              "\nType :set <strategy> to change the current strategy." ^
              "\nType :env to see the current definitions." ^
              "\nType <var> = <term> to assing a varible to a term." ^
              "\nType <term> to evaluate a term." ^
            "\nType :t <id> to know the type.");
           st

       | Envm ->
           List.iter
             (fun (v,t) ->
                print_endline (v ^ " = " ^ show_term t))
             st.env;
           st

       | Set s ->
        (match string_to_strategy s with
         | Some ev ->
             print_endline ("Evaluator set to " ^ s);
             { st with eval = ev }

         | None ->
             print_endline ("Unknown evaluator: " ^ s);
         st)

       | Type x ->
           print_endline "Type not implemented yet";
           st
        | Load file ->
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
            st')

  | Assign (v, t) ->
      let t' = expand st.env t in
      print_endline ("Assigned " ^ v ^ " = " ^ show_term t');
      { st with env = (v,t') :: st.env }

  | Term t ->
      let t' = expand st.env t in
      print_endline @@ show_term t';
      print_endline "Evaluating...";
      let t'' = eval st.eval t' in
      (match term_of_int t'' with
        | Some n -> print_endline @@ string_of_int n
      | None -> print_endline @@ show_term t'');
      st

let prompt = "λ> "

let rec loop st =
  print_string prompt;
  flush stdout;

  match read_line () with
  | exception End_of_file ->
      print_endline "\nBye!"

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
