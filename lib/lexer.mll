{

type token =
  | NUM of int
  | PLUS
  | EOL

exception Eof

let string_of_token = function
| NUM num -> Printf.sprintf "NUM(%d)" num
| PLUS -> "PLUS"
| EOL -> "EOL"

}

rule token = parse
    [' ' '\t']          { token lexbuf }
  | ['\n']              { EOL }
  | ['0'-'9']+ as lxm   { NUM (int_of_string lxm) }
  | '+'                 { PLUS }
  | eof                 { raise Eof }