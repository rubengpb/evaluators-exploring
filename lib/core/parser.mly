%{
open Syntax
%}

%token LPAREN
%token RPAREN
%token LAMBDA
%token DOT
%token <string> IDENT
%token <int> NUMBER

%start <Syntax.term> main

%%

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
