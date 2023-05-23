exception LoopBreak

let driver(): Token.token list =
  let lst = ref [] in
    try
      let lexbuf = Lexing.from_channel stdin in
      while true do
        let result = Lexer.token lexbuf in
          lst := !lst @ [result]
      done;
      raise LoopBreak
    with Lexer.Eof ->
      !lst