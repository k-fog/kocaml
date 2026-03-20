let emitf buf fmt = Printf.bprintf buf (fmt ^^ "\n")

let gen tokens =
  let buf = Buffer.create 256 in
  let open Lexer in
  let rec emit_ops = function
    | [ Eof ] -> ()
    | Plus :: Int n :: rest ->
        emitf buf "  add rax, %d" n;
        emit_ops rest
    | Minus :: Int n :: rest ->
        emitf buf "  sub rax, %d" n;
        emit_ops rest
    | _ -> failwith "expected '+' or '-' followed by an integer"
  in
  match tokens with
  | Int n :: rest ->
      emitf buf ".intel_syntax noprefix";
      emitf buf ".globl main";
      emitf buf "main:";
      emitf buf "  mov rax, %d" n;
      emit_ops rest;
      emitf buf "  ret";
      Buffer.contents buf
  | _ -> failwith "expected an integer"

let () = read_line () |> Lexer.tokenize |> gen |> print_string
