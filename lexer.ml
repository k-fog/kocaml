type t = { src : string; mutable pos : int }

let is_space = function ' ' | '\012' | '\n' | '\r' | '\t' -> true | _ -> false
let is_digit c = '0' <= c && c <= '9'
let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'
let peek l = l.src.[l.pos]
let advance l = l.pos <- l.pos + 1

let skip_space l =
  let len = String.length l.src in
  while l.pos < len && is_space l.src.[l.pos] do
    advance l
  done

let next_token l =
  skip_space l;
  let len = String.length l.src in
  let open Token in
  if len <= l.pos then make Eof l.pos l.pos
  else
    let start = l.pos in
    match peek l with
    | '+' ->
        advance l;
        make Plus start l.pos
    | '-' ->
        advance l;
        if peek l = '>' then (
          advance l;
          make RArrow start l.pos)
        else make Minus start l.pos
    | '*' ->
        advance l;
        make Star start l.pos
    | '/' ->
        advance l;
        make Slash start l.pos
    | '=' ->
        advance l;
        make Equal start l.pos
    | '(' ->
        advance l;
        make LParen start l.pos
    | ')' ->
        advance l;
        make RParen start l.pos
    | c when is_digit c ->
        let start = l.pos in
        while l.pos < len && is_digit l.src.[l.pos] do
          advance l
        done;
        let n = int_of_string (String.sub l.src start (l.pos - start)) in
        make (Int n) start l.pos
    | c when is_letter c -> (
        let start = l.pos in
        while
          l.pos < len && (is_letter l.src.[l.pos] || is_digit l.src.[l.pos])
        do
          advance l
        done;
        let sym = String.sub l.src start (l.pos - start) in
        let kw_kind = List.assoc_opt sym Token.keywords in
        match kw_kind with
        | Some kw -> make kw start l.pos
        | None -> make (Var sym) start l.pos)
    | _ ->
        Error.raise_lex (Span.make l.pos l.pos)
          (Printf.sprintf "invalid character: %c" l.src.[l.pos])

let rec tokenize s =
  let l = { src = s; pos = 0 } in
  let rec aux acc =
    let tok = next_token l in
    match tok.kind with
    | Token.Eof -> List.rev (tok :: acc)
    | _ -> aux (tok :: acc)
  in
  aux []
