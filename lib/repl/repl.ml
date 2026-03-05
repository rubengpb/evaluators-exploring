open Parser
open Lexer
open Ast
open Core.Syntax
open Evals.Main_eval

let token_to_string = function
  | Q -> "Q"
  | INFO -> "INFO"
  | SET -> "SET"
  | TYPE -> "TYPE"
  | H -> "H"
  | ENVM -> "ENVM"
  | EQUAL -> "EQUAL"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LAMBDA -> "LAMBDA"
  | DOT -> "DOT"
  | IDENT s -> "IDENT(" ^ s ^ ")"
  | COLON -> "COLON"
  | EOF -> "EOF"

let rec print_tokens lexbuf =
  let tok = read lexbuf in
  print_endline (token_to_string tok);
  if tok <> EOF then print_tokens lexbuf

let parse line =
  let lexbuf = Lexing.from_string line in
  repl read lexbuf

let handle_command = function
  | Instr i ->
      (match i with
       | Quit -> print_endline "Bye!"; exit 0
       | Info -> print_endline "Normal Order reduction!"
       | Set -> print_endline "Set!"
       | Type -> print_endline "Type!"
       | Help -> print_endline "Simple λ-REPL: type lambda-terms, assing lambda-terms and use it. Type :q for exit"
       | Envm -> print_endline "Envm!"
      )
  | Assign (v, t) ->
      print_endline ("Assigned " ^ v ^ " = " ^ show_term t)
  | Term t ->
      let t' = eval Normal t in
      print_endline ("Evaluated term: " ^ show_term t')

let prompt = "λ> "

let rec loop () =
  print_string prompt;
  flush stdout;
  match read_line () with
  | exception End_of_file ->
      print_endline "\nBye!"
  | line ->
      (try
         let ast = parse line in
      (* print_endline @@ "Parsed input: [[ " ^ (show_command ast) ^ " ]]" *)
          handle_command ast
       with
       | Failure msg -> print_endline ("Failure: " ^ msg)
       | Parser.Error -> print_endline "Parse error");
      loop ()
