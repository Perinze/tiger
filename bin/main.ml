open Tiger

let () =
  let args = Sys.argv in
  let argc = Array.length args in
  let channel =
    match argc with
    | 1 -> stdin
    | _ -> open_in args.(1)
  in
  Parzer.parse Lexer.token (Lexing.from_channel channel)
  |> Semant.trans_prog