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
