let mkbuf s =
  Lexing.from_string s

let tokens s =
  let lst = ref [] in
    let lexbuf = mkbuf s in
      let loop = ref true in
        while !loop do
          let result = Lexer.token lexbuf in
            lst := !lst @ [result];
            if result = Parzer.EOF then
              loop := false
        done;
      !lst

let test s =
  Parzer.parse Lexer.token (mkbuf s)