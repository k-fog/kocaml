type token_stream = { tokens : Token.t array; pos : int }

let make_token_stream toks = { tokens = Array.of_list toks; pos = 0 }
let peek st = st.tokens.(st.pos)

let advance st =
  let t = peek st in
  if t.kind = Token.Eof then st else { st with pos = st.pos + 1 }

let consume st =
  let t = peek st in
  (t, advance st)

let expect expected st =
  let t = peek st in
  if t.kind = expected then (t, advance st)
  else
    Error.raise_parse t.span
      (Printf.sprintf "expected %s but got %s"
         (Token.string_of_kind expected)
         (Token.string_of_kind t.kind))

let rec parse_atom st =
  let tok, st = consume st in
  match tok.kind with
  | Token.Int n -> (Ast.int n tok.span, st)
  | Token.Bool b -> (Ast.bool b tok.span, st)
  | Token.Var var -> (Ast.var var tok.span, st)
  | Token.LParen ->
      let e, st = parse_expr st in
      let rpar, st = expect Token.RParen st in
      (Ast.with_span e (Span.merge tok.span rpar.span), st)
  | _ -> Error.raise_parse tok.span "expected expression"

and parse_app st =
  let rec loop fn st =
    match (peek st).kind with
    | Token.Int _ | Token.Bool _ | Token.Var _ | Token.LParen ->
        let arg, st = parse_atom st in
        loop (Ast.app fn arg) st
    | _ -> (fn, st)
  in
  let fn, st = parse_atom st in
  loop fn st

and parse_prefix st =
  match (peek st).kind with
  | Token.Plus ->
      let tok, st = consume st in
      let rhs, st = parse_prefix st in
      (Ast.with_span rhs (Span.merge tok.span rhs.span), st)
  | Token.Minus ->
      let tok, st = consume st in
      let rhs, st = parse_prefix st in
      (Ast.neg rhs (Span.merge tok.span rhs.span), st)
  | _ -> parse_app st

and parse_mul st =
  let lhs, st = parse_prefix st in
  let rec loop lhs st =
    match (peek st).kind with
    | Token.Star ->
        let _, st = consume st in
        let rhs, st = parse_prefix st in
        loop (Ast.bin_expr Mul lhs rhs) st
    | Token.Slash ->
        let _, st = consume st in
        let rhs, st = parse_prefix st in
        loop (Ast.bin_expr Div lhs rhs) st
    | _ -> (lhs, st)
  in
  loop lhs st

and parse_add st =
  let lhs, st = parse_mul st in
  let rec loop lhs st =
    match (peek st).kind with
    | Token.Plus ->
        let _, st = consume st in
        let rhs, st = parse_mul st in
        loop (Ast.bin_expr Add lhs rhs) st
    | Token.Minus ->
        let _, st = consume st in
        let rhs, st = parse_mul st in
        loop (Ast.bin_expr Sub lhs rhs) st
    | _ -> (lhs, st)
  in
  loop lhs st

and parse_let st =
  let start, st = expect Token.Let st in
  let tok, st = consume st in
  match tok.kind with
  | Token.Var var ->
      let _, st = expect Token.Equal st in
      let e1, st = parse_expr st in
      let _, st = expect Token.In st in
      let e2, st = parse_expr st in
      (Ast.let_expr var e1 e2 (Span.merge start.span e2.span), st)
  | _ -> Error.raise_parse tok.span "expected identifier"

and parse_fun st =
  let start, st = expect Token.Fun st in
  let tok, st = consume st in
  match tok.kind with
  | Token.Var var ->
      let _, st = expect Token.RArrow st in
      let e, st = parse_expr st in
      (Ast.fun_expr var e (Span.merge start.span e.span), st)
  | _ -> Error.raise_parse tok.span "expected identifier"

and parse_cmp st =
  let lhs, st = parse_add st in
  match (peek st).kind with
  | Token.LAngle ->
      let _, st = consume st in
      let rhs, st = parse_add st in
      (Ast.bin_expr Lt lhs rhs, st)
  | Token.RAngle ->
      let _, st = consume st in
      let rhs, st = parse_add st in
      (Ast.bin_expr Lt rhs lhs, st)
  | Token.LAngleEqual ->
      let _, st = consume st in
      let rhs, st = parse_add st in
      (Ast.bin_expr Le lhs rhs, st)
  | Token.RAngleEqual ->
      let _, st = consume st in
      let rhs, st = parse_add st in
      (Ast.bin_expr Le rhs lhs, st)
  | Token.LRAngle ->
      let _, st = consume st in
      let rhs, st = parse_add st in
      (Ast.bin_expr Neq lhs rhs, st)
  | Token.Equal ->
      let _, st = consume st in
      let rhs, st = parse_add st in
      (Ast.bin_expr Eq lhs rhs, st)
  | _ -> (lhs, st)

and parse_if st =
  match (peek st).kind with
  | Token.If ->
      let tok, st = consume st in
      let cond, st = parse_expr st in
      let _, st = expect Token.Then st in
      let e1, st = parse_expr st in
      let _, st = expect Token.Else st in
      let e2, st = parse_expr st in
      (Ast.if_expr cond e1 e2 (Span.merge tok.span e2.span), st)
  | _ -> parse_cmp st

and parse_let_or_fun st =
  match (peek st).kind with
  | Token.Let -> parse_let st
  | Token.Fun -> parse_fun st
  | _ -> parse_if st

and parse_expr st = parse_let_or_fun st

let parse tokens =
  let st = make_token_stream tokens in
  let e, st = parse_expr st in
  match (peek st).kind with
  | Token.Eof -> e
  | tok ->
      Error.raise_parse (peek st).span
        (Printf.sprintf "unexpected token: %s" (Token.string_of_kind tok))
