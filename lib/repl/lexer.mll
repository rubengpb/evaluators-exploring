{
open Parser
}

rule read = parse
  | [' ' '\t' '\n'] { read lexbuf }

  | ":q" { Q }
  | ":info" { INFO }
  | ":set" { SET }

  | "nor" { NOR }
  | "aor" { AOR }

  | "=" { EQUAL }

  | "(" { LPAREN }
  | ")" { RPAREN }

  | "\\" { LAMBDA }
  | "." { DOT }

  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
      { IDENT (Lexing.lexeme lexbuf) }

  | eof { EOF }

  | _ {
      failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf)
    }
