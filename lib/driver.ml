exception LoopBreak

let driver(): Token.token list =
  let lst = ref [] in
    let lexbuf = Lexing.from_channel stdin in
    let continue = ref true in
    while !continue do
      let result = Lexer.token lexbuf in
        if result = Token.EOF then
          continue := false;
        lst := !lst @ [result]
    done;
    !lst