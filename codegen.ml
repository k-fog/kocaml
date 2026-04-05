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

let make_label prefix = Printf.sprintf "%s.%d" prefix (Id.gen ())

type state = { funs : Buffer.t }

let rec gen_expr st buf ast env =
  let open Ast in
  match ast.desc with
  | Int n -> emitf buf "  push %d" n
  | Bool b -> emitf buf "  push %d" (Bool.to_int b)
  | Var var ->
      if not (Env.mem env var) then
        let var = Alpha.restore var in
        Error.raise_codegen ast.span (Printf.sprintf "unbound value %s" var)
      else
        let offset = Env.find env var in
        emitf buf "  mov rax, [rbp-%d]" offset;
        emitf buf "  push rax"
  | UnaryExpr (op, rhs) -> (
      gen_expr st buf rhs env;
      emitf buf "  pop rax";
      match op with
      | Neg ->
          emitf buf "  neg rax";
          emitf buf "  push rax"
      | Not ->
          emitf buf "  cmp rax, 0";
          emitf buf "  sete al";
          emitf buf "  movzx eax, al";
          emitf buf "  push rax")
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
          emitf buf "  idiv rdi"
      | Lt ->
          emitf buf "  cmp rax, rdi";
          emitf buf "  setl al";
          emitf buf "  movzb eax, al"
      | Le ->
          emitf buf "  cmp rax, rdi";
          emitf buf "  setle al";
          emitf buf "  movzb eax, al"
      | Eq ->
          emitf buf "  cmp rax, rdi";
          emitf buf "  sete al";
          emitf buf "  movzb eax, al"
      | Neq ->
          emitf buf "  cmp rax, rdi";
          emitf buf "  setne al";
          emitf buf "  movzb eax, al");
      emitf buf "  push rax"
  | And (e1, e2) ->
      let label = make_label "endand" in
      gen_expr st buf e1 env;
      emitf buf "  pop rax";
      emitf buf "  cmp rax, 0";
      emitf buf "  je %s" label;
      gen_expr st buf e2 env;
      emitf buf "  pop rax";
      emitf buf "  cmp rax, 0";
      emitf buf "%s:" label;
      emitf buf "  setne al";
      emitf buf "  movzb rax, al";
      emitf buf "  push rax"
  | Or (e1, e2) ->
      let label = make_label "endor" in
      gen_expr st buf e1 env;
      emitf buf "  pop rax";
      emitf buf "  cmp rax, 0";
      emitf buf "  jne %s" label;
      gen_expr st buf e2 env;
      emitf buf "  pop rax";
      emitf buf "  cmp rax, 0";
      emitf buf "%s:" label;
      emitf buf "  setne al";
      emitf buf "  movzb rax, al";
      emitf buf "  push rax"
  | If (cond, e1, e2) ->
      gen_expr st buf cond env;
      emitf buf "  pop rax";
      emitf buf "  test rax, rax";
      let id = Id.gen () in
      emitf buf "  je else_%d" id;
      emitf buf "then_%d:" id;
      gen_expr st buf e1 env;
      emitf buf "  jmp endif_%d" id;
      emitf buf "else_%d:" id;
      gen_expr st buf e2 env;
      emitf buf "endif_%d:" id
  | Let (var, e1, e2) ->
      gen_expr st buf e1 env;
      emitf buf "  pop rax";
      let env' = Env.copy env in
      let offset = Env.add_local env' var in
      emitf buf "  mov [rbp-%d], rax" offset;
      gen_expr st buf e2 env'
  | LetRec (var, param, body, e) ->
      let label = make_label "letrec" in
      let fn_env = Env.create () in
      let self_offset = Env.add_local fn_env var in
      let param_offset = Env.add_local fn_env param in
      let fn_body = Buffer.create 256 in
      gen_expr st fn_body body fn_env;
      emitf st.funs "%s:" label;
      emitf st.funs "  push rbp";
      emitf st.funs "  mov rbp, rsp";
      emitf st.funs "  sub rsp, %d" fn_env.frame.size;
      emitf st.funs "  lea rax, [rip+%s]" label;
      emitf st.funs "  mov [rbp-%d], rax" self_offset;
      emitf st.funs "  mov [rbp-%d], rdi" param_offset;
      Buffer.add_buffer st.funs fn_body;
      emitf st.funs "  pop rax";
      emitf st.funs "  mov rsp, rbp";
      emitf st.funs "  pop rbp";
      emitf st.funs "  ret";
      let env' = Env.copy env in
      let var_offset = Env.add_local env' var in
      emitf buf "  lea rax, [rip+%s]" label;
      emitf buf "  mov [rbp-%d], rax" var_offset;
      gen_expr st buf e env'
  | Fun (param, body) ->
      let label = make_label "lambda" in
      let fn_env = Env.create () in
      let param_offset = Env.add_local fn_env param in
      let fn_body = Buffer.create 256 in
      gen_expr st fn_body body fn_env;
      emitf st.funs "%s:" label;
      emitf st.funs "  push rbp";
      emitf st.funs "  mov rbp, rsp";
      emitf st.funs "  sub rsp, %d" fn_env.frame.size;
      emitf st.funs "  mov [rbp-%d], rdi" param_offset;
      Buffer.add_buffer st.funs fn_body;
      emitf st.funs "  pop rax";
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
