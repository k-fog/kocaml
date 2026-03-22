type t = { src : string; mutable pos : int }

let is_space = function ' ' | '\012' | '\n' | '\r' | '\t' -> true | _ -> false
let is_digit c = '0' <= c && c <= '9'

let skip_space l =
  let len = String.length l.src in
  while l.pos < len && is_space l.src.[l.pos] do
    l.pos <- l.pos + 1
  done

let next_token l =
  skip_space l;
  let len = String.length l.src in
  let open Token in
  if len <= l.pos then make Eof l.pos l.pos
  else
    let start = l.pos in
    match l.src.[l.pos] with
    | '+' ->
        l.pos <- l.pos + 1;
        make Plus start l.pos
    | '-' ->
        l.pos <- l.pos + 1;
        make Minus start l.pos
    | '*' ->
        l.pos <- l.pos + 1;
        make Star start l.pos
    | '/' ->
        l.pos <- l.pos + 1;
        make Slash start l.pos
    | '(' ->
        l.pos <- l.pos + 1;
        make LParen start l.pos
    | ')' ->
        l.pos <- l.pos + 1;
        make RParen start l.pos
    | c when is_digit c ->
        let start = l.pos in
        while l.pos < len && is_digit l.src.[l.pos] do
          l.pos <- l.pos + 1
        done;
        let n = int_of_string (String.sub l.src start (l.pos - start)) in
        make (Int n) start l.pos
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
