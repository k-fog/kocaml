let emitf buf fmt = Printf.bprintf buf (fmt ^^ "\n")

let gen ast =
  let buf = Buffer.create 256 in
  let open Ast in
  let rec gen_expr ast =
    match ast with
    | Int n -> emitf buf "  push %d" n
    | BinExpr (op, lhs, rhs) ->
        gen_expr lhs;
        gen_expr rhs;
        emitf buf "  pop rdi";
        emitf buf "  pop rax";
        (match op with
        | Add -> emitf buf "  add rax, rdi"
        | Sub -> emitf buf "  sub rax, rdi"
        | Mul -> emitf buf "  imul rax, rdi"
        | Div ->
            emitf buf "  cqo";
            emitf buf "  idiv rdi");
        emitf buf "  push rax"
  in
  emitf buf ".intel_syntax noprefix";
  emitf buf ".globl main";
  emitf buf "main:";
  gen_expr ast;
  emitf buf "  pop rax";
  emitf buf "  ret";
  Buffer.contents buf
