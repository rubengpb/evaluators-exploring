{
open Parser
}

rule read = parse
  | [' ' '\t' '\n'] { read lexbuf }

  | ":q" { Q }
  | ":config" { ICONFIG }
  | ":set" { SET }
  | ":env" { ENV }
  | ":expEnv" { EXENV1 }
  | ":expandedEnv" { EXENV2 }
  | ":t" { TYPE }
  | ":h" { H }
  | ":?" { H }
  | ":load" { LOAD }

  | "=" { EQUAL }

  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACK }
  | "]" { RBRACK }

  | "\\" { LAMBDA }
  | "." { DOT }
  | ";" { SEMICO }

  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
      { IDENT (Lexing.lexeme lexbuf) }

  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '/']*
      { FILENAME (Lexing.lexeme lexbuf) }

  | ['0'-'9']+ { NUMBER (int_of_string @@ Lexing.lexeme lexbuf) }
  | eof { EOF }

  | _ {
      failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf)
    }
