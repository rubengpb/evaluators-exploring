%{
open Ast
open Core.Syntax
open Core.Church_numerals
%}

%token Q
%token ICONFIG
%token SET
%token TYPE
%token H
%token LOAD
%token <string> FILENAME
%token EQUAL
%token LPAREN
%token RPAREN
%token LAMBDA
%token DOT
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
  | NUMBER { term_of_int $1 }
  | LPAREN term RPAREN { $2 }
