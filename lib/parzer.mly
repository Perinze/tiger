/* File parzer.mly */
%{
  let error tokidx = Errormsg.error (rhs_start tokidx)
%}
%token EOF
%token <string> ID
%token <int> INT
%token <string> STRING
%token COMMA COLON SEMICOLON
%token LPAREN RPAREN LBRACK RBRACK
%token LBRACE RBRACE DOT
%token PLUS MINUS TIMES DIVIDE
%token EQ NEQ LT LE GT GE
%token AND OR ASSIGN
%token ARRAY IF THEN ELSE
%token WHILE FOR TO DO LET IN END OF
%token BREAK NIL
%token FUNCTION VAR TYPE

%nonassoc DO THEN OF
%nonassoc ELSE
%nonassoc ASSIGN
%left AND OR
%left EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start parse
%type <unit> parse

%%
parse:
    exp EOF {}
;

/* DECLARATIONS */

/* decs -> { dec } */
decs:
    /* empty */ {}
  | dec decs {}
;

dec:
    tydec {}
  | vardec {}
  | fundec {}
;

/* DATA TYPES */

tydec:
    TYPE ID EQ ty {}
  | TYPE ID EQ ty tydec {}
;

ty:
    ID {}
  | LBRACE tyfields RBRACE {}
  | ARRAY OF ID {}
;

/* tyfields -> epsilon
 *          -> id: type-id {, id: type-id} */
tyfields:
    /* empty */ {}
  | ID COLON ID tyfields_ {}
;
tyfields_:
    /* empty */ {}
  | COMMA ID COLON ID tyfields_ {}
;

/* VARIABLES */

vardec:
    VAR ID ASSIGN exp {}
  | VAR ID COLON ID ASSIGN exp {}
;

/* FUNCTIONS */

fundec:
    FUNCTION ID LPAREN tyfields RPAREN EQ exp {}
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp {}
  | FUNCTION ID LPAREN tyfields RPAREN EQ exp fundec {}
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp fundec {}
;

/* VARIABLES AND EXPRESSIONS */

/* L-VALUES */

lvalue:
    ID {}
  | ID DOT ID {}
  | ID LBRACK exp RBRACK {}
  | lvalue DOT ID {}
  | lvalue LBRACK exp RBRACK {}
;

/* exp; exp; ...exp */
seq:
    exp {}
  | exp SEMICOLON seq {}
;

/* exp {, exp} */
params:
    exp {}
  | exp COMMA params {}
;

op:
    PLUS {}
  | MINUS {}
  | TIMES {}
  | DIVIDE {}
  | EQ {}
  | NEQ {}
  | GT {}
  | LT {}
  | GE {}
  | LE {}
  | AND {}
  | OR {}
;

/* type-id {id=exp{, id=exp}} */
record_creation:
    ID LBRACE RBRACE {}
  | ID LBRACE field_assign RBRACE {}
;
field_assign:
    ID EQ exp {}
  | ID EQ exp COMMA field_assign {}
;

/* type-id [exp1] of exp2 */
array_creation:
    ID LBRACK exp RBRACK OF exp {}
;

exp:
    lvalue {}
  | NIL { }
  | LPAREN seq RPAREN {}
  | LPAREN RPAREN {}
  | LPAREN error RPAREN { error 2 "sequence error" }
  | LPAREN error { error 1 "unmatched parentheses" }
  | error RPAREN { error 2 "unmatched parentheses" }
  | LET decs IN END {}
  | LET error IN END { error 2 "declaration error" }
  | INT { }
  | STRING { }
  | MINUS exp %prec UMINUS {}
  | ID LPAREN RPAREN {}
  | ID LPAREN params RPAREN {}
  | ID LPAREN error RPAREN { error 3 "argument syntax error" }
  | ID LPAREN error { error 2 "unmatched parentheses" }
  | ID error RPAREN { error 3 "unmatched parentheses" }
  | exp op exp {}
  | op exp { error 1 "missing left operand" }
  | exp op { error 2 "missing right operand" }
  | record_creation {}
  | array_creation {}
  | lvalue ASSIGN exp {}
  | error ASSIGN exp { error 1 "lvalue error in assignment" }
  | IF exp THEN exp ELSE exp {}
  | IF exp THEN exp {}
  | WHILE exp DO exp {}
  | FOR ID ASSIGN exp TO exp DO exp {}
  | BREAK {}
  | LET decs IN seq END {}
  | error { error 1 "unexpected expression" }
;