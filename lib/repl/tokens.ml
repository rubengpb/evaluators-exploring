open Lexer
open Parser

let token_to_string = function
  | Q -> "Q"
  | ICONFIG -> "ICONFIG"
  | SET -> "SET"
  | ENV -> "ENV"
  | EXENV1 -> "EXENV1"
  | EXENV2 -> "EXENV2"
  | TYPE -> "TYPE"
  | H -> "H"
  | LOAD -> "LOAD"
  | FILENAME s -> "FILENAME(" ^ s ^ ")"
  | EQUAL -> "EQUAL"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACK -> "LBRACK"
  | RBRACK -> "RBRACK"
  | LAMBDA -> "LAMBDA"
  | DOT -> "DOT"
  | SEMICO -> "SEMICO"
  | IDENT s -> "IDENT(" ^ s ^ ")"
  | NUMBER n -> "NUMBER(" ^ (string_of_int n) ^ ")"
  | EOF -> "EOF"

let rec print_tokens lexbuf =
  let tok = read lexbuf in
  print_endline (token_to_string tok);
  if tok <> EOF then print_tokens lexbuf
