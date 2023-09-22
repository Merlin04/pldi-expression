%{

open Ast

%}

%token <int> Int
%token <string> Id

%token Plus
%token Dash
%token Star
%token Slash
%token LAngle
%token Eq
%token Amp
%token Pipe
%token Not
%token Mod
%token If
%token Then
%token Else
%token Let
%token In
%token True
%token False
%token LParen
%token RParen
%token EOF

%start <expr> start

%type <expr> expr

%left Pipe
%left Amp
%left Eq LAngle
%left Plus Dash
%left Star Slash Mod

%nonassoc Not

%%
start :
  | p = expr; EOF { p }

expr :
  | a = expr; Pipe; b = expr { Or (a, b) }
  | a = expr; Amp; b = expr { And (a, b) }
  | a = expr; Eq; b = expr { Equal (a, b) }
  | a = expr; LAngle; b = expr { Lt (a, b) }
  | a = expr; Plus; b = expr { Add (a, b) }
  | a = expr; Dash; b = expr { Sub (a, b) }
  | a = expr; Star; b = expr { Mul (a, b) }
  | a = expr; Slash; b = expr { Div (a, b) }
  | a = expr; Mod; b = expr { ModExp (a, b) }
  | Not; a = expr { NotExp a }
  | n = Int { Int n }
  | i = Id { Id i }
  | True { Boolean true }
  | False { Boolean false }
  | LParen; e = expr; RParen { e }