{

open Parser

let sb = Buffer.create 256

exception SyntaxError of string

}

let whitespace = [' ' '\t' '\r' '\n']+
let id = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let int = ['0'-'9']+

rule tok = parse
  | whitespace { tok lexbuf }
  | '+' { Plus }
  | '-' { Dash }
  | '*' { Star }
  | '/' { Slash }
  | '<' { LAngle }
  | '=' { Eq }
  | '&' { Amp }
  | '|' { Pipe }
  | "not" { Not }
  | "mod" { Mod }
  | "if" { If }
  | "then" { Then }
  | "else" { Else }
  | "let" { Let }
  | "in" { In }
  | "true" { True }
  | "false" { False }
  | "(" { LParen }
  | ")" { RParen }
  | int as i { Int (int_of_string i) }
  | id as s { Id s }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

{

let tokenize (s : string) : token list =
  let buf = Lexing.from_string s in
  let rec helper acc =
    match tok buf with
      | EOF -> List.rev acc
      | t -> helper (t :: acc) in
  helper []

}