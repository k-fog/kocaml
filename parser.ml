type token_stream = { tokens : Token.t array; mutable pos : int }

let make_token_stream toks = { tokens = Array.of_list toks; pos = 0 }
let peek st = st.tokens.(st.pos)

let advance st =
  if st.tokens.(st.pos).kind <> Token.Eof then st.pos <- st.pos + 1

let consume st =
  let t = peek st in
  if t.kind <> Eof then st.pos <- st.pos + 1;
  t

let expect st expected =
  let t = peek st in
  if t.kind = expected then advance st
  else
    Error.raise_parse t.span
      (Printf.sprintf "expected %s but got %s"
         (Token.string_of_kind expected)
         (Token.string_of_kind t.kind))

let precedence token_kind =
  let open Token in
  match token_kind with Plus | Minus -> 2 | Star | Slash -> 3 | _ -> 0

let is_right_assoc tok = match tok with _ -> false

let is_binop token_kind =
  let open Token in
  match token_kind with Plus | Minus | Star | Slash -> true | _ -> false

let rec parse_prefix st =
  let tok = peek st in
  match tok.kind with
  | Token.Int n ->
      advance st;
      Ast.int n tok.span
  | Token.Var var ->
      advance st;
      Ast.var var tok.span
  | Token.LParen ->
      advance st;
      let e = parse_expr st in
      expect st Token.RParen;
      e
  | Token.Plus ->
      advance st;
      parse_prefix st
  | Token.Minus ->
      advance st;
      let e = parse_prefix st in
      Ast.neg e (Span.merge tok.span e.span)
  | Token.Let -> parse_let st
  | _ -> Error.raise_parse tok.span "cannot parse"

and parse_binop st min_prec =
  let rec loop lhs =
    let tok = peek st in
    let op = tok.kind in
    if is_binop op then
      let prec = precedence op in
      if prec < min_prec then lhs
      else (
        advance st;
        let next_min_prec = if is_right_assoc op then prec else prec + 1 in
        let rhs = parse_binop st next_min_prec in
        let lhs' =
          match op with
          | Token.Plus -> Ast.bin_expr Add lhs rhs
          | Token.Minus -> Ast.bin_expr Sub lhs rhs
          | Token.Star -> Ast.bin_expr Mul lhs rhs
          | Token.Slash -> Ast.bin_expr Div lhs rhs
          | _ -> Error.raise_parse tok.span "internal error"
        in
        loop lhs')
    else lhs
  in
  let lhs = parse_prefix st in
  loop lhs

and parse_let st =
  expect st Token.Let;
  let tok = consume st in
  match tok.kind with
  | Token.Var var ->
      expect st Token.Equal;
      let e1 = parse_expr st in
      expect st Token.In;
      let e2 = parse_expr st in
      Ast.let_expr var e1 e2 (Span.merge tok.span e2.span)
  | _ -> Error.raise_parse tok.span "expected identifier"

and parse_expr st = parse_binop st 0
and parse tokens = tokens |> make_token_stream |> parse_expr
