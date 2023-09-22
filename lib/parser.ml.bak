open Lexer

exception ParseError of string

type expr =
  | Or of expr * expr
  | And of expr * expr
  | Equal of expr * expr
  | Lt of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | ModExp of expr * expr
  | NotExp of expr
  | Boolean of bool
  | Id of string
  | Int of int

let rec print_expr : expr -> string = function
  | Or (e1, e2) -> "(" ^ print_expr e1 ^ ") | (" ^ print_expr e2 ^ ")"
  | And (e1, e2) -> "(" ^ print_expr e1 ^ ") & (" ^ print_expr e2 ^ ")"
  | Equal (e1, e2) -> "(" ^ print_expr e1 ^ ") = (" ^ print_expr e2 ^ ")"
  | Lt (e1, e2) -> "(" ^ print_expr e1 ^ ") < (" ^ print_expr e2 ^ ")"
  | NotExp e -> "not (" ^ print_expr e ^ ")"
  | Boolean b -> string_of_bool b
  | Id s -> s
  | Sub (e1, e2) -> "(" ^ print_expr e1 ^ ") - (" ^ print_expr e2 ^ ")"
  | Div (e1, e2) -> "(" ^ print_expr e1 ^ ") / (" ^ print_expr e2 ^ ")"
  | ModExp (e1, e2) -> "(" ^ print_expr e1 ^ ") % (" ^ print_expr e2 ^ ")"
  | Add (e1, e2) -> "(" ^ print_expr e1 ^ ") + (" ^ print_expr e2 ^ ")"
  | Mul (e1, e2) -> "(" ^ print_expr e1 ^ ") * (" ^ print_expr e2 ^ ")"
  | Int i -> string_of_int i

(* All of the parsing functions here have type
   token list -> expr * token list. The idea here is that the function takes a
   stream of tokens, parses whatever it's supposed to parse off the front and
   returns that expression along with the rest of the token list which was not
   consumed. For example,

   bexpr [Lit 5; Plus; Lit 3] = (Int 5, [Plus; Lit 3])

   because the bexpr nonterminal parses an integer. The remaining tokens, Plus
   and Lit 3 are just returned. Similarly,

   mexpr [Lit 4; Times; Lit 2; Plus; Lit 1] =
       (Mul (Int 4, Int 2), [Plus; Lit 1])
  *)

type parser_res = (expr * token list)
type parser_fn = token list -> parser_res

(* parser combinator for dyadic left-associative operation(s). [next] is the parser with next highest precedence. *)
let dyadic_lassc_parser (ops : (token * (expr -> expr -> expr)) list) (next : parser_fn) (s : token list) : parser_res =
  let rec try_add (e : expr) (r : token list) = match r with
    | h :: r2 ->
      (match List.find_opt (fun (t, _) -> t = h) ops with
        | Some((_, c)) -> let e2, r3 = next r2 in try_add (c e e2) r3
        | None -> e, r)
    | _ -> e, r in
  let e, r = next s in
  try_add e r

(* unfortunately can't use the parser combinator directly (by partial application) because of recursive definitions. if you know a way around this lmk! *)
let rec parse_expr : parser_fn = fun s -> dyadic_lassc_parser [(Pipe, fun a b -> Or (a, b))] and_exp s (* https://github.com/ocaml/ocaml/pull/9005 :( *)
and and_exp : parser_fn = fun s -> dyadic_lassc_parser [(Amp, fun a b -> And (a, b))] eq_exp s
and eq_exp : parser_fn = fun s -> dyadic_lassc_parser [(Eq, fun a b -> Equal (a, b)); (LAngle, fun a b -> Lt (a, b))] add_exp s
and add_exp : parser_fn = fun s -> dyadic_lassc_parser [(Plus, fun a b -> Add (a, b)); (Dash, fun a b -> Sub (a, b))] mul_exp s
and mul_exp : parser_fn = fun s -> dyadic_lassc_parser [(Star, fun a b -> Mul (a, b)); (Slash, fun a b -> Div (a, b)); (Mod, fun a b -> ModExp (a, b))] unary_exp s
and unary_exp : parser_fn = function
  | Not :: r -> let e, r2 = unary_exp r in (NotExp (e), r2)
  | r -> literal_exp r
and literal_exp : parser_fn = function
  | Lit i :: r -> Int i, r
  | Var s :: r -> Id s, r
  | True :: r -> Boolean true, r
  | False :: r -> Boolean false, r
  | r -> paren_exp r
and paren_exp : parser_fn = function
  | LParen :: r -> let e, r2 = parse_expr r in (match r2 with
    | RParen :: r3 -> e, r3
    | [] -> raise (ParseError "Expected ), got end of input")
    | t :: _ -> raise (ParseError ("Expected ), got " ^ tok_to_str t)))
  | [] -> raise (ParseError "Expected literal or (, got end of input")
  | t :: _ -> raise (ParseError ("Expected literal or (, got " ^ tok_to_str t))

let parse (s : token list) : expr =
  match parse_expr s with
    | e, [] -> e
    | _, t :: _ -> raise (ParseError ("Expected end of input, got " ^ tok_to_str t))