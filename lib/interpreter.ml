open Parser

exception RuntimeError of string

type value =
  | VInt of int
  | VBool of bool

let print_value : value -> string = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b

let rec interpret_with_ctx (c : (string * value) list) (interp_expr : expr) : value =
  let interpret = interpret_with_ctx c in
  let interp_bool (a : expr) (op : string) = match interpret a with
    | VBool b -> b
    | _ -> raise (RuntimeError ("Non-boolean argument " ^ (print_expr a) ^ " to " ^ op)) in
  let interp_int (a : expr) (op : string) = match interpret a with
    | VInt i -> i
    | _ -> raise (RuntimeError ("Non-integer argument " ^ (print_expr a) ^ " to " ^ op)) in
  let dyadic_bool (e1 : expr) (e2 : expr) (f : bool -> bool -> bool) (name : string) : value =
    VBool (f (interp_bool e1 name) (interp_bool e2 name)) in
  let dyadic_int (e1 : expr) (e2 : expr) (f : int -> int -> int) (name : string) : value =
    VInt (f (interp_int e1 name) (interp_int e2 name)) in
  match interp_expr with
    | Or (e1, e2) -> dyadic_bool e1 e2 (||) "|"
    | And (e1, e2) -> dyadic_bool e1 e2 (&&) "&"
    | Equal (e1, e2) -> VBool ((interpret e1) = (interpret e2))
    | Lt (e1, e2) -> let c = "<" in VBool ((interp_int e1 c) < (interp_int e2 c))
    | Add (e1, e2) -> dyadic_int e1 e2 (+) "+"
    | Sub (e1, e2) -> dyadic_int e1 e2 (-) "-"
    | Mul (e1, e2) -> dyadic_int e1 e2 ( * ) "*"
    | Div (e1, e2) -> dyadic_int e1 e2 (/) "/"
    | ModExp (e1, e2) -> dyadic_int e1 e2 (mod) "mod"
    | NotExp e -> VBool (not (interp_bool e "not"))
    | Boolean b -> VBool b
    | Int i -> VInt i
    | Id a -> (match List.assoc_opt a c with Some v -> v | None -> raise (RuntimeError ("Unbound variable " ^ a)))
    | IfExp (cond, a, b) -> if (interp_bool cond "if condition") then interpret a else interpret b
    | LetExp (id, value, p) -> interpret_with_ctx ((id, interpret value) :: c) p

 let interpret = interpret_with_ctx []