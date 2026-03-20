exception Error of string * Lexer.span

let raise_at span msg = raise (Error (msg, span))

let report_error src (msg, span) =
  let open Lexer in
  prerr_endline src;
  prerr_endline
    (String.make span.start ' '
    ^ String.make (max 1 (span.finish - span.start)) '^');
  prerr_endline ("error: " ^ msg)

let emitf buf fmt = Printf.bprintf buf (fmt ^^ "\n")

let gen src tokens =
  let open Lexer in
  let buf = Buffer.create 256 in
  let rec emit_ops = function
    | [ { kind = Eof; _ } ] -> ()
    | { kind = Plus; _ } :: { kind = Int n; _ } :: rest ->
        emitf buf "  add rax, %d" n;
        emit_ops rest
    | { kind = Minus; _ } :: { kind = Int n; _ } :: rest ->
        emitf buf "  sub rax, %d" n;
        emit_ops rest
    | tok :: _ -> raise_at tok.span "expected '+' or '-' followed by an integer"
    | [] ->
        raise_at
          { start = String.length src; finish = String.length src }
          "unexpected end of input"
  in
  match tokens with
  | { kind = Int n; _ } :: rest ->
      emitf buf ".intel_syntax noprefix";
      emitf buf ".globl main";
      emitf buf "main:";
      emitf buf "  mov rax, %d" n;
      emit_ops rest;
      emitf buf "  ret";
      Buffer.contents buf
  | { kind = Eof; span } :: _ -> raise_at span "expected an integer"
  | tok :: _ -> raise_at tok.span "expected an integer at the beginning"
  | [] -> raise_at { start = 0; finish = 0 } "no tokens"

let () =
  let src = read_line () in
  try src |> Lexer.tokenize |> gen src |> print_endline
  with Error (msg, span) -> report_error src (msg, span)
