{
open Token
exception Eof
}

rule token = parse
    [' ' '\t' '\n']     { token lexbuf }
  | ['0'-'9']+ as lxm   { INT (int_of_string lxm) }
  | '+'                 { PLUS }
  | eof                 { raise Eof }