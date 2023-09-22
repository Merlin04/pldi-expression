open Ast

let parse_lexbuf (b : Lexing.lexbuf) : expr =
  Parser.start Lexer.tok b

let parse_string s = parse_lexbuf (Lexing.from_string s)

let parse_file fn =
  In_channel.with_open_text fn (fun i -> parse_lexbuf (Lexing.from_channel i))
