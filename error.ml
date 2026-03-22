type t = LexError of string * Span.t | ParseError of string * Span.t

exception Error of t

let raise_lex span msg = raise (Error (LexError (msg, span)))
let raise_parse span msg = raise (Error (ParseError (msg, span)))

let report src (msg, (span : Span.t)) =
  prerr_endline src;
  prerr_endline
    (String.make span.start ' '
    ^ String.make (max 1 (span.finish - span.start)) '^');
  prerr_endline ("error: " ^ msg)
