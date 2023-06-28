let mkbuf s =
  Lexing.from_string s

let lex lexbuf =
  let lst = ref [] in
    let loop = ref true in
    while !loop do
      let result = Lexer.token lexbuf in
      lst := !lst @ [result];
      if result = Parzer.EOF then
        loop := false
    done;
    !lst

let lex_string s =
  lex (mkbuf s)

let lex_channel c =
  lex (Lexing.from_channel c)

let parze lexbuf =
  Parzer.parse Lexer.token lexbuf

let parze_string s =
  parze (mkbuf s)

let parze_channel c =
  parze (Lexing.from_channel c)

let type_check_string s =
  parze_string s |> Semant.trans_prog

let type_check_channel c =
  parze_channel c |> Semant.trans_prog