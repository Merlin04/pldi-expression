open Lexer
open Parser
open Interpreter

let run a = a |> tokenize |> parse |> interpret