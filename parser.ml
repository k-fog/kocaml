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
  if t.kind = expected then (
    advance st;
    t)
  else
    Error.raise_parse t.span
      (Printf.sprintf "expected %s but got %s"
         (Token.string_of_kind expected)
         (Token.string_of_kind t.kind))

let rec parse_atom st =
  let tok = consume st in
  match tok.kind with
  | Token.Int n -> Ast.int n tok.span
  | Token.Var var -> Ast.var var tok.span
  | Token.LParen ->
      let e = parse_expr st in
      let rpar = expect st Token.RParen in
      Ast.with_span e (Span.merge tok.span rpar.span)
  | _ -> Error.raise_parse tok.span "expected expression"

and parse_app st =
  let rec loop fn =
    let tok = peek st in
    match tok.kind with
    | Token.Int _ | Token.Var _ | Token.LParen ->
        let arg = parse_atom st in
        let fn = Ast.app fn arg in
        loop fn
    | _ -> fn
  in
  let fn = parse_atom st in
  loop fn

and parse_prefix st =
  let tok = peek st in
  match tok.kind with
  | Token.Plus ->
      let _op = consume st in
      let rhs = parse_prefix st in
      Ast.with_span rhs (Span.merge tok.span rhs.span)
  | Token.Minus ->
      let _op = consume st in
      let rhs = parse_prefix st in
      Ast.neg rhs (Span.merge tok.span rhs.span)
  | _ -> parse_app st

and parse_mul st =
  let rec loop lhs =
    let tok = peek st in
    match tok.kind with
    | Token.Star ->
        let _op = consume st in
        let rhs = parse_mul st in
        let lhs = Ast.bin_expr Mul lhs rhs in
        loop lhs
    | Token.Slash ->
        let _op = consume st in
        let rhs = parse_mul st in
        let lhs = Ast.bin_expr Div lhs rhs in
        loop lhs
    | _ -> lhs
  in
  let lhs = parse_prefix st in
  loop lhs

and parse_add st =
  let rec loop lhs =
    let tok = peek st in
    match tok.kind with
    | Token.Plus ->
        let _op = consume st in
        let rhs = parse_mul st in
        let lhs = Ast.bin_expr Add lhs rhs in
        loop lhs
    | Token.Minus ->
        let _op = consume st in
        let rhs = parse_mul st in
        let lhs = Ast.bin_expr Sub lhs rhs in
        loop lhs
    | _ -> lhs
  in
  let lhs = parse_mul st in
  loop lhs

and parse_let st =
  let start = expect st Token.Let in
  let tok = consume st in
  match tok.kind with
  | Token.Var var ->
      let _eq = expect st Token.Equal in
      let e1 = parse_expr st in
      let _in = expect st Token.In in
      let e2 = parse_expr st in
      Ast.let_expr var e1 e2 (Span.merge start.span e2.span)
  | _ -> Error.raise_parse tok.span "expected identifier"

and parse_fun st =
  let start = expect st Token.Fun in
  let tok = consume st in
  match tok.kind with
  | Var var ->
      let _ra = expect st Token.RArrow in
      let e = parse_expr st in
      Ast.fun_expr var e (Span.merge start.span e.span)
  | _ -> Error.raise_parse tok.span "expected identifier"

and parse_let_or_fun st =
  let tok = peek st in
  match tok.kind with
  | Token.Let -> parse_let st
  | Token.Fun -> parse_fun st
  | _ -> parse_add st

and parse_expr st = parse_let_or_fun st
and parse tokens = tokens |> make_token_stream |> parse_expr
