type kind =
  | Int of int
  | Var of string
  | Plus
  | Minus
  | Star
  | Slash
  | Equal
  | LParen
  | RParen
  | Let
  | In
  | Eof

type t = { kind : kind; span : Span.t }

let make kind start finish = { kind; span = { start; finish } }
let keywords = [ ("let", Let); ("in", In) ]

let string_of_kind = function
  | Int n -> string_of_int n
  | Var id -> id
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Equal -> "="
  | LParen -> "("
  | RParen -> ")"
  | Let -> "let"
  | In -> "in"
  | Eof -> "<EOF>"
