open Parser
open Lexer
open Ast

let token_to_string = function
  | Q -> "Q"
  | INFO -> "INFO"
  | SET -> "SET"
  | NOR -> "NOR"
  | AOR -> "AOR"
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
      print_endline @@ "Parsed input: [[ " ^ (show_command ast) ^ " ]]"
       with
       | Failure msg -> print_endline ("Failure: " ^ msg)
       | Parser.Error -> print_endline "Parse error");
      loop ()
