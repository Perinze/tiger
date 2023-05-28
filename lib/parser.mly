/* File parser.mly */
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

%left AND OR
%left EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start parse
%type <unit> parse

%token TYDEC VARDEC FUNDEC

%%
parse:
    decs EOF {}
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
  | tyfield {}
  | tyfield COMMA tyfields {}
;
tyfield:
    ID COLON ID {}
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
  | lvalue DOT ID {}
  | lvalue LBRACK exp RBRACK {}
;

/* exp; exp; ...exp */
seq:
    exp SEMICOLON exp {}
  | seq SEMICOLON exp {}
;

/* exp {, exp} */
params:
    exp {}
  | params COMMA exp {}
;

exp:
    lvalue {}
  | NIL {}
  | LPAREN seq RPAREN {}
  | LPAREN RPAREN {}
  | LET decs IN END {}
  | INT {}
  | STRING {}
  | MINUS exp %prec UMINUS {}
  | ID LPAREN RPAREN {}
  | ID LPAREN params RPAREN {}
  | LET decs IN exp END {}
;