type token_kind =
  | Int of int
  | Plus
  | Minus
  | Star
  | Slash
  | LParen
  | RParen
  | Eof

type t = { kind : token_kind; span : Span.t }

let make kind start finish = { kind; span = { start; finish } }

let string_of_kind = function
  | Int _ -> "<integer>"
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | LParen -> "("
  | RParen -> ")"
  | Eof -> "<EOF>"
