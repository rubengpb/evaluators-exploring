open Parser
open Lexer

let parse line =
  let lexbuf = Lexing.from_string line in
  repl read lexbuf
