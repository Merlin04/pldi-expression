type token =
  | Plus           (** + *)
  | Dash           (** - *)
  | Star           (** * *)
  | Slash          (** / *)
  | LAngle         (** < *)
  | Eq             (** = *)
  | Amp            (** & *)
  | Pipe           (** | *)
  | Not            (** not *)
  | Mod            (** mod *)
  | If             (** if *)
  | Then           (** then *)
  | Else           (** else *)
  | Let            (** let *)
  | In             (** in *)
  | True           (** true *)
  | False          (** false *)
  | LParen         (** ( *)
  | RParen         (** ) *)
  | Lit of int     (** $int *)
  | Var of string  (** $id *)

let tok_to_str : token -> string = function
  | Plus -> "+"
  | Dash -> "-"
  | Star -> "*"
  | Slash -> "/"
  | LAngle -> "<"
  | Eq -> "="
  | Amp -> "&&"
  | Pipe -> "||"
  | Not -> "not"
  | Mod -> "mod"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | Let -> "let"
  | In -> "in"
  | True -> "true"
  | False -> "false"
  | LParen -> "("
  | RParen -> ")"
  | Lit i -> string_of_int i
  | Var s -> s

exception LexError of string

let tokenize (src : string) : token list =
  let is_digit c = '0' <= c && c <= '9' in
  let is_alpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') in
  let consume_int i =
    let rec loop j =
      if j < String.length src && is_digit src.[j] then loop (j + 1)
      else (Lit (int_of_string (String.sub src i (j - i))), j) in
    loop i in
  let consume_word i =
    let rec loop j =
      if j < String.length src && is_alpha src.[j] then loop (j + 1)
      else match String.sub src i (j - i) with
        | "not" -> (Not, j)
        | "mod" -> (Mod, j)
        | "if" -> (If, j)
        | "then" -> (Then, j)
        | "else" -> (Else, j)
        | "let" -> (Let, j)
        | "in" -> (In, j)
        | "true" -> (True, j)
        | "false" -> (False, j)
        | s -> (Var s, j) in
    loop i in
  let rec loop i =
    if i >= String.length src then []
    else match src.[i] with
    | '+' -> Plus :: loop (i + 1)
    | '-' -> Dash :: loop (i + 1)
    | '*' -> Star :: loop (i + 1)
    | '/' -> Slash :: loop (i + 1)
    | '<' -> LAngle :: loop (i + 1)
    | '=' -> Eq :: loop (i + 1)
    | '&' -> Amp :: loop (i + 1)
    | '|' -> Pipe :: loop (i + 1)
    | '(' -> LParen :: loop (i + 1)
    | ')' -> RParen :: loop (i + 1)
    | c when List.mem c ['\n'; '\r'; '\t'; ' '] -> loop (i + 1)
    | c when is_digit c -> let (v, j) = consume_int i in v :: loop j
    | _ -> let (v, j) = consume_word i in v :: loop j in
  loop 0
