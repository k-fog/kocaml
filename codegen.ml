let emitf buf fmt = Printf.bprintf buf (fmt ^^ "\n")

module Env = struct
  type frame = { mutable size : int }

  type t = {
    table : (string, int) Hashtbl.t;
    mutable offset : int;
    frame : frame;
  }

  let create () =
    { table = Hashtbl.create 16; offset = 0; frame = { size = 0 } }

  let copy env =
    { table = Hashtbl.copy env.table; offset = env.offset; frame = env.frame }

  let add_local env name =
    env.offset <- env.offset + 8;
    if env.offset > env.frame.size then env.frame.size <- env.offset;
    Hashtbl.replace env.table name env.offset;
    env.offset

  let find env name = Hashtbl.find env.table name
  let mem env name = Hashtbl.mem env.table name
end

module Label = struct
  let counter = ref 0

  let create () =
    let label = string_of_int !counter in
    incr counter;
    label
end

type state = { funs : Buffer.t }

let rec gen_expr st buf ast env =
  let open Ast in
  match ast.desc with
  | Int n -> emitf buf "  push %d" n
  | Var var ->
      if not (Env.mem env var) then
        Error.raise_codegen ast.span (Printf.sprintf "unbound variable: %s" var);
      let offset = Env.find env var in
      emitf buf "  mov rax, [rbp-%d]" offset;
      emitf buf "  push rax"
  | Neg e ->
      gen_expr st buf e env;
      emitf buf "  pop rax";
      emitf buf "  neg rax";
      emitf buf "  push rax"
  | BinExpr (op, lhs, rhs) ->
      gen_expr st buf lhs env;
      gen_expr st buf rhs env;
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
      gen_expr st buf e1 env;
      emitf buf "  pop rax";
      let env' = Env.copy env in
      let offset = Env.add_local env' var in
      emitf buf "  mov [rbp-%d], rax" offset;
      gen_expr st buf e2 env'
  | Fun (var, body) ->
      let label = "lambda_" ^ Label.create () in
      let fn_env = Env.create () in
      let arg_offset = Env.add_local fn_env var in
      let fn_body = Buffer.create 256 in
      gen_expr st fn_body body fn_env;
      emitf fn_body "  pop rax";
      emitf st.funs "%s:" label;
      emitf st.funs "  push rbp";
      emitf st.funs "  mov rbp, rsp";
      emitf st.funs "  sub rsp, %d" fn_env.frame.size;
      emitf st.funs "  mov [rbp-%d], rdi" arg_offset;
      Buffer.add_buffer st.funs fn_body;
      emitf st.funs "  mov rsp, rbp";
      emitf st.funs "  pop rbp";
      emitf st.funs "  ret";
      emitf buf "  lea rax, [rip+%s]" label;
      emitf buf "  push rax"
  | App (fn, arg) ->
      gen_expr st buf fn env;
      gen_expr st buf arg env;
      emitf buf "  pop rdi";
      emitf buf "  pop rax";
      emitf buf "  call rax";
      emitf buf "  push rax"

let gen ast =
  let st = { funs = Buffer.create 256 } in
  let main_env = Env.create () in
  let main_buf = Buffer.create 256 in
  gen_expr st main_buf ast main_env;
  emitf main_buf "  pop rax";
  emitf main_buf "  mov rsp, rbp";
  emitf main_buf "  pop rbp";
  emitf main_buf "  ret";

  let out = Buffer.create 512 in
  emitf out ".intel_syntax noprefix";
  emitf out ".text";
  emitf out ".globl main";
  Buffer.add_buffer out st.funs;
  emitf out "main:";
  emitf out "  push rbp";
  emitf out "  mov rbp, rsp";
  emitf out "  sub rsp, %d" main_env.frame.size;
  Buffer.add_buffer out main_buf;
  Buffer.contents out
