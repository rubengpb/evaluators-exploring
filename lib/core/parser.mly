%{
open Syntax
open Church_numerals
open Church_list
%}

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

%start <Syntax.pterm> main

%%

main:
  | term EOF { $1 }

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
