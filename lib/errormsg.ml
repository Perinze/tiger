let any_errors = ref false

exception Error

let error (pos : Lexing.position) (msg : string) : unit =
  any_errors := true;
  print_endline pos.pos_fname;
  print_endline (":" ^ string_of_int pos.pos_lnum ^
                 "." ^ string_of_int (pos.pos_cnum - pos.pos_bol));
  print_endline (":" ^ msg)

let impossible msg =
  print_endline ("error: compiler bug: " ^ msg);
  raise Error