open OUnit2
open Expression.Lexer
open Expression.Parser
open Expression.Interpreter

let print_list (show : 'a -> string) (lst : 'a list) : string =
  "[" ^ String.concat ", " (List.map show lst) ^ "]"

let lex_tests = "test suite for lexer" >::: [
    "all tokens" >::
    (fun _ -> assert_equal ~printer:(print_list tok_to_str)
        [Plus; Dash; Star; Slash; LAngle; Eq; Amp; Pipe; Not; Mod; If; Then;
         Else; Let; In; True; False; LParen; RParen; Lit 1; Var "x"]
        (tokenize "+-*/<=&|not mod if then else let in true false()1 x"));
    "abs" >::
    (fun _ -> assert_equal ~printer:(print_list tok_to_str)
        [If; Var "x"; LAngle; Lit 0; Then; Lit 0; Dash; Var "x"; Else; Var "x"]
        (tokenize "if x < 0 then 0 - x else x"));
  ]

let parse_tests = "test suite for parser" >::: [
    "plus" >::
    (fun _ -> assert_equal ~printer:print_expr
        (Add (Int 1, Int 2))
        (parse (tokenize "1 + 2")));
    "associativity" >::
    (fun _ -> assert_equal ~printer:print_expr
        (Add (Add (Int 1, Int 2), Int 3))
        (parse (tokenize "1 + 2 + 3")));
    "parentheses" >::
    (fun _ -> assert_equal ~printer:print_expr
        (Add (Add (Int 1, Int 2), Add (Int 3, Int 4)))
        (parse (tokenize "1 + 2 + (3 + 4)")));
    "times" >::
    (fun _ -> assert_equal ~printer:print_expr
        (Mul (Mul (Int 1, Int 2), Mul (Int 3, Int 4)))
        (parse (tokenize "1 * 2 * (3 * 4)")));
    "precedence" >::
    (fun _ -> assert_equal ~printer:print_expr
        (Add (Mul (Int 1, Int 2), Mul (Int 3, Int 4)))
        (parse (tokenize "1 * 2 + 3 * 4")));
    "hanging operator" >::
    (fun _ -> try
        let _ = parse (tokenize "1 + 3 +") in
        assert_failure "'1 + 3 +' passed the parser"
      with
      | ParseError _ -> assert_bool "" true
      | _ -> assert_failure "Unexpected error");
    "leading operator" >::
    (fun _ -> try
        let _ = parse (tokenize "* 1 + 3") in
        assert_failure "'* 1 + 3' passed the parser"
      with
      | ParseError _ -> assert_bool "" true
      | _ -> assert_failure "Unexpected error");
  ]

let interp_str (s : string) : value =
  interpret (parse (tokenize s))

let interp_tests = "test suite for the interpreter" >::: [
    "plus" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 3)
        (interp_str "1 + 2"));
    "times" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 10)
        (interp_str "5 * 2"));
  ]

let sub_div_mod_tests = "test suite for -, /, and mod" >::: [
    "subtract associativity" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 0)
        (interp_str "3 - 2 - 1"));
    "subtract parentheses" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 2)
        (interp_str "3 - (2 - 1)"));
    "division" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 2)
        (interp_str "8 / 4"));
    "integer division" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 2)
        (interp_str "7 / 3"));
    "division precedence" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 0)
        (interp_str "3 / 2 - 1"));
    "modulo" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 1)
        (interp_str "7 mod 3"));
    "mod precedence" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 1)
        (interp_str "8 mod 2 + 1"));
    "multiplication, division, and mod precedence and associativity" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 3)
        (interp_str "8 mod 3 * (7 / 3) mod 3 * 10 / 3"));
  ]

let compare_tests = "test suite for < and =" >::: [
    "less than true" >::
    (fun _ -> assert_equal ~printer:print_value
        (VBool true)
        (interp_str "2 < 3"));
    "less than false" >::
    (fun _ -> assert_equal ~printer:print_value
        (VBool false)
        (interp_str "2 < 2"));
    "equal true" >::
    (fun _ -> assert_equal ~printer:print_value
        (VBool true)
        (interp_str "2 = 2"));
    "equal false" >::
    (fun _ -> assert_equal ~printer:print_value
        (VBool false)
        (interp_str "2 = 3"));
    "less than precedence" >::
    (fun _ -> assert_equal ~printer:print_value
        (VBool true)
        (interp_str "3 / 2 < 2"));
    "equal precedence" >::
    (fun _ -> assert_equal ~printer:print_value
        (VBool false)
        (interp_str "4 + 3 = 3"));
  ]

let bool_tests = "test suite for boolean constants, &, and |" >::: [
    "and true" >::
    (fun _ -> assert_equal ~printer:print_value
        (VBool true)
        (interp_str "true & true"));
    "and false" >::
    (fun _ -> assert_equal ~printer:print_value
        (VBool false)
        (interp_str "false & true"));
    "or true" >::
    (fun _ -> assert_equal ~printer:print_value
        (VBool true)
        (interp_str "true | false"));
    "or false" >::
    (fun _ -> assert_equal ~printer:print_value
        (VBool false)
        (interp_str "false | false"));
    "and/or precedence" >::
    (fun _ -> assert_equal ~printer:print_value
        (VBool true)
        (interp_str "true | true & false"));
    "boolean precedence w.r.t. arithmetic" >::
    (fun _ -> assert_equal ~printer:print_value
        (VBool true)
        (interp_str "2 + 3 < 5 | 3 < 4 & 5 = 5"));
  ]

let if_tests = "test suite for if-expressions" >::: [
    "if tree" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 2)
        (interp_str ("if true " ^
                     "then if false then 1 else 2 " ^
                     "else if false then 3 else 4")));
    "if as expression" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 2)
        (interp_str "1 + if 2 = 1 then 2 else 1"));
    "if must have else" >::
    (fun _ -> try
        let _ = parse (tokenize "if 2 = 1 then 2") in
        assert_failure "Parsed an if without an else"
      with
      | ParseError _ -> assert_bool "" true
      | _ -> assert_failure "Unexpected error");
  ]

let let_tests = "test suite for let-expressions" >::: [
    "basic let" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 2)
        (interp_str "let x = 1 in x + 1"));
    "multiple let" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 2)
        (interp_str "let x = 1 in let y = 3 in y - x"));
    "let inside binding" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 2)
        (interp_str "let x = let y = 3 in y - 1 in 1 * x"));
    "shadowing" >::
    (fun _ -> assert_equal ~printer:print_value
        (VInt 2)
        (interp_str "let x = 1 in let x = 2 in x"));
  ]

let all_tests = "full test suite for expressions" >::: [
    lex_tests;
    parse_tests;
 (* interp_tests;
    sub_div_mod_tests;
    compare_tests;
    bool_tests;
    if_tests;
    let_tests; *)
  ]

let _ = run_test_tt_main all_tests
