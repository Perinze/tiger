{
}
let letter = ['_' 'a'-'z' 'A'-'Z']
let digit = ['0'-'9']
rule token = parse
    [' ' '\t' '\n'] { token lexbuf }
  | "type"          { TYPE }
  | "var"           { VAR }
  | "function"      { FUNCTION }
  | "break"         { BREAK }   
  | "of"            { OF }
  | "end"           { END }
  | "in"            { IN }
  | "nil"           { NIL }
  | "let"           { LET }
  | "do"            { DO }
  | "to"            { TO }
  | "for"           { FOR }
  | "while"         { WHILE }
  | "else"          { ELSE }
  | "then"          { THEN }
  | "if"            { IF }
  | "array"         { ARRAY }
  | ":="            { ASSIGN }
  | "|"             { OR }
  | "&"             { AND }
  | ">="            { GE }
  | ">"             { GT }
  | "<="            { LE }
  | "<"             { LT }
  | "<>"            { NEQ }
  | "="             { EQ }
  | "/"             { DIVIDE }
  | "*"             { TIMES }
  | "-"             { MINUS }
  | "+"             { PLUS }
  | "."             { DOT }
  | "}"             { RBRACE }
  | "{"             { LBRACE }
  | "]"             { RBRACK }
  | "["             { LBRACK }
  | ")"             { RPAREN }
  | "("             { LPAREN }
  | ";"             { SEMICOLON }
  | ":"             { COLON }
  | ","             { COMMA }
  | '\"'([^'\"']* as lxm)'\"'
                    { STRING lxm }
  | digit+ as lxm
                    { INT (int_of_string lxm) }
  | letter (letter|digit)* as lxm
                    { ID lxm }
  | eof             { EOF }