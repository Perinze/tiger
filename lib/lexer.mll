let letter = ['_' 'a'-'z' 'A'-'Z']
let digit = ['0'-'9']
rule token = parse
    [' ' '\t' '\n'] { token lexbuf }
  | "type"          { Parser.TYPE }
  | "var"           { Parser.VAR }
  | "function"      { Parser.FUNCTION }
  | "break"         { Parser.BREAK }   
  | "of"            { Parser.OF }
  | "end"           { Parser.END }
  | "in"            { Parser.IN }
  | "nil"           { Parser.NIL }
  | "let"           { Parser.LET }
  | "do"            { Parser.DO }
  | "to"            { Parser.TO }
  | "for"           { Parser.FOR }
  | "while"         { Parser.WHILE }
  | "else"          { Parser.ELSE }
  | "then"          { Parser.THEN }
  | "if"            { Parser.IF }
  | "array"         { Parser.ARRAY }
  | ":="            { Parser.ASSIGN }
  | "|"             { Parser.OR }
  | "&"             { Parser.AND }
  | ">="            { Parser.GE }
  | ">"             { Parser.GT }
  | "<="            { Parser.LE }
  | "<"             { Parser.LT }
  | "<>"            { Parser.NEQ }
  | "="             { Parser.EQ }
  | "/"             { Parser.DIVIDE }
  | "*"             { Parser.TIMES }
  | "-"             { Parser.MINUS }
  | "+"             { Parser.PLUS }
  | "."             { Parser.DOT }
  | "}"             { Parser.RBRACE }
  | "{"             { Parser.LBRACE }
  | "]"             { Parser.RBRACK }
  | "["             { Parser.LBRACK }
  | ")"             { Parser.RPAREN }
  | "("             { Parser.LPAREN }
  | ";"             { Parser.SEMICOLON }
  | ":"             { Parser.COLON }
  | ","             { Parser.COMMA }
  | '\"'([^'\"']* as lxm)'\"'
                    { Parser.STRING lxm }
  | digit+ as lxm
                    { Parser.INT (int_of_string lxm) }
  | letter (letter|digit)* as lxm
                    { Parser.ID lxm }
  | eof             { Parser.EOF }