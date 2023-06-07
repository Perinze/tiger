/* File parzer.mly */
%{
open Absyn
let sym = Symbol.symbol
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
%type <Absyn.exp> parse

%%
parse:
    exp EOF { $1 }
;

/* DECLARATIONS */

/* decs -> { dec } */
decs:
    /* empty */ { [] }
  | dec decs { $1 :: $2 }
;

dec:
    tydec { $1 }
  | vardec { $1 }
  | fundec { $1 }
;

/* DATA TYPES */

tydec:
    TYPE ID EQ ty tydec_ { TypeDec ({
      name=sym $2; ty=$4; pos=rhs_start 1
    } :: $5) }
;
tydec_:
    /* empty */ { [] }
  | TYPE ID EQ ty tydec_ { {
      name=sym $2; ty=$4; pos=rhs_start 1
    } :: $5 }
;

ty:
    ID { NameTy (sym $1, rhs_start 1) }
  | LBRACE tyfields RBRACE { RecordTy $2 }
  | ARRAY OF ID { ArrayTy (sym $3, rhs_start 1) }
;

/* tyfields -> epsilon
 *          -> id: type-id {, id: type-id} */
tyfields:
    /* empty */ { [] }
  | ID COLON ID tyfields_ {
    { name=sym $1; escape=ref true;
      typ=sym $3; pos=rhs_start 2 } :: $4
  }
;
tyfields_:
    /* empty */ { [] }
  | COMMA ID COLON ID tyfields_ {
    { name=sym $2; escape=ref true;
      typ=sym $4; pos=rhs_start 3 } :: $5
  }
;

/* VARIABLES */

vardec:
    VAR ID ASSIGN exp { VarDec {
      name=sym $2; escape=ref true;
      typ=None; init=$4; pos=rhs_start 1;
    } }
  | VAR ID COLON ID ASSIGN exp { VarDec {
      name=sym $2; escape=ref true;
      typ=Some (sym $4, rhs_start 4);
      init=$6; pos=rhs_start 1;
    } }
;

/* FUNCTIONS */

fundec:
    FUNCTION ID LPAREN tyfields RPAREN EQ exp fundec_ { FunctionDec ({
      name=sym $2; params=$4; result=None;
      body=$7; pos=rhs_start 1;
    } :: $8) }
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp fundec_ { FunctionDec ({
      name=sym $2; params=$4;
      result=Some (sym $7, rhs_start 7);
      body=$9; pos=rhs_start 1;
    } :: $10) }
;
fundec_:
    /* empty */ { [] }
  | FUNCTION ID LPAREN tyfields RPAREN EQ exp fundec_ { {
      name=sym $2; params=$4; result=None;
      body=$7; pos=rhs_start 1;
    } :: $8 }
  | FUNCTION ID LPAREN tyfields RPAREN COLON ID EQ exp fundec_ { {
      name=sym $2; params=$4;
      result=Some (sym $7, rhs_start 7);
      body=$9; pos=rhs_start 1;
    } :: $10 }
;

/* VARIABLES AND EXPRESSIONS */

/* L-VALUES */

lvalue:
    ID { SimpleVar (sym $1, rhs_start 1) }
  | ID DOT ID {
      FieldVar (
        SimpleVar (sym $1, rhs_start 1),
        sym $3,
        rhs_start 3
      )
    }
  | ID LBRACK exp RBRACK {
    SubscriptVar (
      SimpleVar (sym $1, rhs_start 1),
      $3,
      rhs_start 2
    )
  }
  | lvalue DOT ID {
    FieldVar ( $1, sym $3, rhs_start 3 )
  }
  | lvalue LBRACK exp RBRACK {
    SubscriptVar ( $1, $3, rhs_start 2 )
  }
;

/* exp; exp; ...exp */
seq:
    exp { [($1, rhs_start 1)] }
  | exp SEMICOLON seq { ($1, rhs_start 1) :: $3 }
;

/* exp {, exp} */
params:
    exp { [$1] }
  | exp COMMA params { $1 :: $3 }
;

/* type-id {id=exp{, id=exp}} */
record_creation:
    ID LBRACE RBRACE {
      RecordExp {
        fields=[]; typ=sym $1; pos=rhs_start 2;
      }
    }
  | ID LBRACE field_assign RBRACE {
      RecordExp {
        fields=$3;
        typ=sym $1; pos=rhs_start 2;
      }
    }
;
field_assign:
    ID EQ exp { [(sym $1, $3, rhs_start 2)]}
  | ID EQ exp COMMA field_assign {
      (sym $1, $3, rhs_start 2) :: $5
    }
;

/* type-id [exp1] of exp2 */
array_creation:
    ID LBRACK exp RBRACK OF exp {
      ArrayExp {
        typ=sym $1; size=$3;
        init=$6; pos=rhs_start 2;
      }
    }
;

exp:
    lvalue { VarExp $1 }
  | NIL { NilExp }
  | LPAREN seq RPAREN { SeqExp $2 }
  | LPAREN RPAREN { UnitExp }
  | LET decs IN END {
      LetExp {
        decs=$2; body=UnitExp;
        pos=rhs_start 1;
      }
    }
  | INT { IntExp $1 }
  | STRING { StringExp ($1, rhs_start 1) }
  | MINUS exp %prec UMINUS {
      OpExp {
        left=IntExp 0; oper=MinusOp;
        right=$2; pos=rhs_start 1;
      }
    }
  | ID LPAREN RPAREN {
      CallExp {
        func=sym $1; args=[]; pos=rhs_start 2;
      }
    }
  | ID LPAREN params RPAREN {
      CallExp {
        func=sym $1; args=$3; pos=rhs_start 2;
      }
    }
  | exp PLUS exp {
      OpExp {
        left=$1; oper=PlusOp;
        right=$3; pos=rhs_start 2;
      }
    }
  | exp MINUS exp {
      OpExp {
        left=$1; oper=MinusOp;
        right=$3; pos=rhs_start 2;
      }
    }
  | exp TIMES exp {
      OpExp {
        left=$1; oper=TimesOp;
        right=$3; pos=rhs_start 2;
      }
    }
  | exp DIVIDE exp {
      OpExp {
        left=$1; oper=DivideOp;
        right=$3; pos=rhs_start 2;
      }
    }
  | exp EQ exp {
      OpExp {
        left=$1; oper=EqOp;
        right=$3; pos=rhs_start 2;
      }
    }
  | exp NEQ exp {
      OpExp {
        left=$1; oper=NeqOp;
        right=$3; pos=rhs_start 2;
      }
    }
  | exp GT exp {
      OpExp {
        left=$1; oper=GtOp;
        right=$3; pos=rhs_start 2;
      }
    }
  | exp LT exp {
      OpExp {
        left=$1; oper=LtOp;
        right=$3; pos=rhs_start 2;
      }
    }
  | exp GE exp {
      OpExp {
        left=$1; oper=GeOp;
        right=$3; pos=rhs_start 2;
      }
    }
  | exp LE exp {
      OpExp {
        left=$1; oper=LeOp;
        right=$3; pos=rhs_start 2;
      }
    }
  | exp AND exp {
      OpExp {
        left=$1; oper=TimesOp;
        right=$3; pos=rhs_start 2;
      }
    }
  | exp OR exp {
      OpExp {
        left=$1; oper=PlusOp;
        right=$3; pos=rhs_start 2;
      }
    }
  | record_creation { $1 }
  | array_creation { $1 }
  | lvalue ASSIGN exp {
      AssignExp {
        var=$1; exp=$3; pos=rhs_start 2;
      }
    }
  | IF exp THEN exp ELSE exp {
      IfExp {
        test=$2; then'=$4;
        else'=Some $6; pos=rhs_start 1;
      }
    }
  | IF exp THEN exp {
      IfExp {
        test=$2; then'=$4;
        else'=None; pos=rhs_start 1;
      }
    }
  | WHILE exp DO exp {
      WhileExp {
        test=$2; body=$4; pos=rhs_start 1;
      }
    }
  | FOR ID ASSIGN exp TO exp DO exp {
      ForExp {
        var=sym $2; escape=ref true;
        lo=$4; hi=$6; body=$8; pos=rhs_start 1;
      }
    }
  | BREAK { BreakExp (rhs_start 1)}
  | LET decs IN seq END {
      LetExp {
        decs=$2; body=SeqExp $4; pos=rhs_start 1;
      }
    }
/*  | LPAREN exp RPAREN {}*/
;