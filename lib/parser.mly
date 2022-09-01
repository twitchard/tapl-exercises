%{
  open Syntax.Named
  exception UnknownType of string
  let parseType (s: string) = if (String.equal s "Nat") then Syntax.Nat else raise (UnknownType s)
%}
%token SUCC
%token PRED
%token ZERO
%token LAMBDA
%token ARROW
%token EOF
%token LPAREN
%token RPAREN
%token PERIOD
%token COLON
%token LET
%token EQUALS
%token AS
%token IN
%token <string> VAR
%token PREC


%type <Syntax.Named.term> prog
%left SUCC
%left PRED
%left PREC
%start prog

%%

prog:
  | term EOF { $1 }
  ;

term:
  | LAMBDA VAR COLON typeexpression PERIOD term   { TmAbs($2, $4, $6) }
  | LET VAR AS typeexpression EQUALS term IN term { TmLet($2, $4, $6, $8) }
  | app                                           { $1 }

app:
  | app atom { TmApp($1, $2) }
  | atom     { $1 }

typeexpression: 
  | VAR                                 { parseType $1 }
  | LPAREN typeexpression RPAREN        { $2 }
  | typeexpression ARROW typeexpression { Syntax.Fn($1, $3) }

atom:
  | ZERO               { TmZero }
  | LPAREN term RPAREN { $2 }
  | VAR                { TmVar $1 }
  | SUCC term          { TmSucc $2 }
  | PRED term          { TmPred $2 }

