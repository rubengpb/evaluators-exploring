%{
open Ast
open Core.Syntax
open Core.Church_numerals
open Core.Church_list
%}

%token Q
%token ICONFIG
%token SET
%token ENV
%token EXENV1
%token EXENV2
%token TYPE
%token H
%token LOAD
%token <string> FILENAME
%token EQUAL
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token LAMBDA
%token DOT
%token SEMICO
%token <string> IDENT
%token <int> NUMBER
%token EOF

%start <Ast.command> repl

%%

repl:
  | instruction EOF { Instr $1 }
  | IDENT EQUAL term EOF { Assign ($1, $3) }
  | term EOF { Term $1 }

instruction:
  | Q { Quit }
  | ICONFIG { IConfig None }
  | ICONFIG IDENT { IConfig (Some $2) }
  | ENV { IConfig (Some "env") }
  | ENV IDENT { IConfig (Some ("env_" ^ $2)) }
  | EXENV1 { IConfig (Some "expEnv") }
  | EXENV1 IDENT { IConfig (Some ("expEnv_" ^ $2)) }
  | EXENV2 { IConfig (Some "expEnv") }
  | EXENV2 IDENT { IConfig (Some ("expEnv_" ^ $2)) }
  | ICONFIG IDENT IDENT { IConfig (Some ($2 ^ "_" ^ $3)) }
  | SET IDENT lident { Set ($2, String.concat "_" $3) }
  | TYPE IDENT { Type $2 }
  | H { Help None }
  | H IDENT { Help (Some $2) }
  | LOAD IDENT { Load $2 }
  | LOAD FILENAME { Load $2 }

lident:
  | IDENT lident { $1 :: $2 }
  | { [] }

term:
  | LAMBDA IDENT DOT term { Abs ($2, $4) }
  | app { $1 }

app:
  | atom atoms { List.fold_left (fun acc t -> App (acc, t)) $1 $2 }

atoms:
  | atom atoms { $1 :: $2 }
  | { [] }

atom:
  | IDENT { Var $1 }
  | NUMBER { pterm_of_int $1 }
  | LBRACK list RBRACK { pterm_of_list $2 }
  | LPAREN term RPAREN { $2 }

list:
  | { [] }
  | term { [$1] }
  | term SEMICO list { $1::$3 }
