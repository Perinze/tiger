exception LoopBreak

let driver(): Parser.token list =
  let lst = ref [] in
    let lexbuf = Lexing.from_channel stdin in
      while true do
        let result = Lexer.token lexbuf in
          lst := !lst @ [result]
      done;
      !lst