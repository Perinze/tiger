open Tiger

let () =
  Parzer.parse Lexer.token (Lexing.from_channel stdin)
  |> Semant.trans_prog