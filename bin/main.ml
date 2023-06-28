open Tiger

let () =
  let args = Sys.argv in
  let argc = Array.length args in
  let channel =
    match argc with
    | 1 -> stdin
    | _ -> open_in args.(1)
  in
  let lexbuf = Lexing.from_channel channel in
  Lexing.set_filename lexbuf args.(1);
  Parzer.parse Lexer.token lexbuf
  |> Semant.trans_prog