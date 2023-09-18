(* open Parser *)

exception RuntimeError of string

type value =
  | VInt of int
  | VBool of bool

let print_value : value -> string = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b

(*let rec interpret : expr -> value = function
  | Add (e1, e2) -> (match (interpret e1, interpret e2) with
    | (VInt i1, VInt i2) -> VInt (i1 + i2)
    | _ -> raise (RuntimeError "Non-integer argument to +"))
  | Mul (e1, e2) -> (match (interpret e1, interpret e2) with
    | (VInt i1, VInt i2) -> VInt (i1 * i2)
    | _ -> raise (RuntimeError "Non-integer argument to *"))
  | Int i -> VInt i
*)

let interpret _expr = raise (RuntimeError "Interpret is not implemented")