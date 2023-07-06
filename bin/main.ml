open Tiger

type arg_parse_next =
| Command
| Src
| Done

let usage_msg : string = {|
Usage: tiger [-h] COMMAND SRC

Commands:
  lex    List tokens (unimplemented)
  abs    Print abstract syntax
  type   Type check
|}
let command = ref ""
let src_file = ref ""
let speclist = []

let () =
  let state = ref Command in
  let anon_fun (name : string) : unit =
    match !state with
    | Command -> begin
      command := name;
      state := Src;
    end
    | Src -> begin
      src_file := name;
      state := Done;
    end
    | Done -> raise (Arg.Bad "unexpected anonymous argument")
  in
  Arg.parse speclist anon_fun usage_msg;
  let channel : in_channel =
    match !state with
    | Command -> (
      Arg.usage speclist usage_msg;
      exit 0;
    )
    | Src -> (
      src_file := "stdin";
      stdin
    )
    | Done -> open_in !src_file
  in
  let lexbuf = Lexing.from_channel channel in
  Lexing.set_filename lexbuf !src_file;

  match !command with
  | "lex" -> print_endline "unimplemented"
  | "abs" -> Util.parze lexbuf |> Absyn.show_exp |> print_endline
  | "type" -> Util.parze lexbuf |> Semant.trans_prog
  | _ -> Arg.usage speclist usage_msg