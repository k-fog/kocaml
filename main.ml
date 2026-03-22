let () =
  let src = read_line () in
  try src |> Lexer.tokenize |> Parser.parse |> Codegen.gen |> print_endline
  with Error.Error e -> (
    match e with
    | Error.LexError (msg, span) -> Error.report src (msg, span)
    | Error.ParseError (msg, span) -> Error.report src (msg, span))
