type kind =
  | Int of int
  | Bool of bool
  | Var of string
  | Plus
  | Minus
  | Star
  | Slash
  | Equal
  | LParen
  | RParen
  | RArrow
  | LAngle
  | RAngle
  | LRAngle
  | LAngleEqual
  | RAngleEqual
  | Let
  | Rec
  | In
  | Fun
  | If
  | Then
  | Else
  | Eof

type t = { kind : kind; span : Span.t }

let make kind start finish = { kind; span = { start; finish } }

let keywords =
  [
    ("let", Let);
    ("rec", Rec);
    ("in", In);
    ("fun", Fun);
    ("if", If);
    ("then", Then);
    ("else", Else);
    ("true", Bool true);
    ("false", Bool false);
  ]

let string_of_kind = function
  | Int n -> string_of_int n
  | Bool b -> if b then "true" else "false"
  | Var id -> id
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Equal -> "="
  | LParen -> "("
  | RParen -> ")"
  | RArrow -> "->"
  | LAngle -> "<"
  | RAngle -> ">"
  | LRAngle -> "<>"
  | LAngleEqual -> "<="
  | RAngleEqual -> ">="
  | Let -> "let"
  | Rec -> "rec"
  | In -> "in"
  | Fun -> "fun"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | Eof -> "<EOF>"
