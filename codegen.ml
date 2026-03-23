let emitf buf fmt = Printf.bprintf buf (fmt ^^ "\n")

module Env = struct
  type t = { table : (string, int) Hashtbl.t; mutable offset : int }

  let create = { table = Hashtbl.create 8; offset = 0 }

  let new_offset env var =
    env.offset <- env.offset + 4;
    Hashtbl.add env.table var env.offset;
    env.offset

  let get_offset env var = Hashtbl.find env.table var
end

let rec gen_expr buf ast env =
  let open Ast in
  match ast.desc with
  | Int n -> emitf buf "  push %d" n
  | Var var ->
      let offset = Env.get_offset env var in
      emitf buf "  mov rax, [rbp-%d]" offset;
      emitf buf "  push rax"
  | Neg e ->
      gen_expr buf e env;
      emitf buf "  pop rax";
      emitf buf "  neg rax";
      emitf buf "  push rax"
  | BinExpr (op, lhs, rhs) ->
      gen_expr buf lhs env;
      gen_expr buf rhs env;
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
  | Let (var, e1, e2) ->
      gen_expr buf e1 env;
      emitf buf "  pop rax";
      let offset = Env.new_offset env var in
      emitf buf "  mov [rbp-%d], rax" offset;
      gen_expr buf e2 env

let gen ast =
  let env = Env.create in
  let buf_main = Buffer.create 256 in
  gen_expr buf_main ast env;
  emitf buf_main "  pop rax";
  emitf buf_main "  mov rsp, rbp";
  emitf buf_main "  pop rbp";
  emitf buf_main "  ret";
  let buf = Buffer.create 256 in
  emitf buf ".intel_syntax noprefix";
  emitf buf ".globl main";
  emitf buf "main:";
  emitf buf "  push rbp";
  emitf buf "  mov rbp, rsp";
  emitf buf "  sub rsp, %d" env.offset;
  Buffer.add_buffer buf buf_main;
  Buffer.contents buf
