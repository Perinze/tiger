/* File parzer.mly */
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
  | LET decs IN END {}
  | INT { }
  | STRING { }
  | MINUS exp %prec UMINUS {}
  | ID LPAREN RPAREN {}
  | ID LPAREN params RPAREN {}
  | exp PLUS exp {}
  | exp MINUS exp {}
  | exp TIMES exp {}
  | exp DIVIDE exp {}
  | exp EQ exp {}
  | exp NEQ exp {}
  | exp GT exp {}
  | exp LT exp {}
  | exp GE exp {}
  | exp LE exp {}
  | exp AND exp {}
  | exp OR exp {}
  | array_creation {}
  | record_creation {}
  | lvalue ASSIGN exp {}
  | IF exp THEN exp ELSE exp {}
  | IF exp THEN exp {}
  | WHILE exp DO exp {}
  | FOR ID ASSIGN exp TO exp DO exp {}
  | BREAK {}
  | LET decs IN exp END {}
  | LPAREN exp RPAREN {}
;