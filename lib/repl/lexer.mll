{
open Parser
}

rule read = parse
  | [' ' '\t' '\n'] { read lexbuf }

  | ":q" { Q }
  | ":info" { INFO }
  | ":set" { SET }
  | ":t" { TYPE }
  | ":h" { H }
  | ":env" { ENVM }
  | ":load" { LOAD }

  | "=" { EQUAL }

  | "(" { LPAREN }
  | ")" { RPAREN }

  | "\\" { LAMBDA }
  | "." { DOT }

  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '/']*
      { IDENT (Lexing.lexeme lexbuf) }

  | ['0'-'9']+ { NUMBER (int_of_string @@ Lexing.lexeme lexbuf) }
  | eof { EOF }

  | _ {
      failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf)
    }
