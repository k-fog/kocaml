type lexer = { src : string; mutable pos : int }
type token = Int of int | Plus | Minus | Eof

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
  if len <= l.pos then Eof
  else
    match l.src.[l.pos] with
    | '+' ->
        l.pos <- l.pos + 1;
        Plus
    | '-' ->
        l.pos <- l.pos + 1;
        Minus
    | c when is_digit c ->
        let start = l.pos in
        while l.pos < len && is_digit l.src.[l.pos] do
          l.pos <- l.pos + 1
        done;
        let n = int_of_string (String.sub l.src start (l.pos - start)) in
        Int n
    | _ -> failwith (Printf.sprintf "invalid character: %c" l.src.[l.pos])

let rec tokenize s =
  let l = { src = s; pos = 0 } in
  let rec aux acc =
    let tok = next_token l in
    match tok with Eof -> List.rev (Eof :: acc) | _ -> aux (tok :: acc)
  in
  aux []
