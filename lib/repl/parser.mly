%{
open Ast
open Core.Syntax
%}

%token Q
%token INFO
%token SET
%token TYPE
%token H
%token ENVM
%token LOAD
%token EQUAL
%token LPAREN
%token RPAREN
%token LAMBDA
%token DOT
%token <string> IDENT
%token EOF

%start <Ast.command> repl

%%

repl:
  | instruction EOF { Instr $1 }
  | IDENT EQUAL term EOF { Assign ($1, $3) }
  | term EOF { Term $1 }

instruction:
  | Q { Quit }
  | INFO { Info }
  | SET IDENT { Set $2 }
  | TYPE IDENT { Type $2 }
  | H { Help }
  | ENVM { Envm }
  | LOAD IDENT { Load $2 }

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
  | LPAREN term RPAREN { $2 }
