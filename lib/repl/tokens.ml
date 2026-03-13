open Lexer
open Parser

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
