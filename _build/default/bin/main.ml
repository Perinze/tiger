open Tiger

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Lexer.token lexbuf in
        Lexer.string_of_token result |> print_endline; flush stdout
    done
  with Lexer.Eof ->
    exit 0