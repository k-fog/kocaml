type lexer = { src : string; mutable pos : int }
type token_kind = Int of int | Plus | Minus | Eof
type span = { start : int; finish : int }
type token = { kind : token_kind; span : span }

let is_space = function ' ' | '\012' | '\n' | '\r' | '\t' -> true | _ -> false
let is_digit c = '0' <= c && c <= '9'
let make_token kind start finish = { kind; span = { start; finish } }

let skip_space l =
  let len = String.length l.src in
  while l.pos < len && is_space l.src.[l.pos] do
    l.pos <- l.pos + 1
  done

let next_token l =
  skip_space l;
  let len = String.length l.src in
  if len <= l.pos then make_token Eof l.pos l.pos
  else
    let start = l.pos in
    match l.src.[l.pos] with
    | '+' ->
        l.pos <- l.pos + 1;
        make_token Plus start l.pos
    | '-' ->
        l.pos <- l.pos + 1;
        make_token Minus start l.pos
    | c when is_digit c ->
        let start = l.pos in
        while l.pos < len && is_digit l.src.[l.pos] do
          l.pos <- l.pos + 1
        done;
        let n = int_of_string (String.sub l.src start (l.pos - start)) in
        make_token (Int n) start l.pos
    | _ -> failwith (Printf.sprintf "invalid character: %c" l.src.[l.pos])

let rec tokenize s =
  let l = { src = s; pos = 0 } in
  let rec aux acc =
    let tok = next_token l in
    match tok.kind with Eof -> List.rev (tok :: acc) | _ -> aux (tok :: acc)
  in
  aux []
