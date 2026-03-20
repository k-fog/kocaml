let emitf buf f = Printf.bprintf buf (f ^^ "\n")

let gen n =
  let buf = Buffer.create 256 in
  emitf buf ".intel_syntax noprefix";
  emitf buf ".globl main";
  emitf buf "main:";
  emitf buf "  mov rax, %d" n;
  emitf buf "  ret";
  Buffer.contents buf

let () = read_line () |> int_of_string |> gen |> print_string
