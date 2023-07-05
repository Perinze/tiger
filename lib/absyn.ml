[@@@ocaml.warning "-30"]
type pos = Lexing.position and symbol = Symbol.symbol

let pp_pos (ppf : Format.formatter) (pos : pos) =
  Format.fprintf ppf "%d.%d" (pos.pos_lnum) (pos.pos_cnum - pos.pos_lnum)
let pp_symbol = Symbol.pp_symbol

type var =
  | SimpleVar of symbol * pos
  | FieldVar of var * symbol * pos
  | SubscriptVar of var * exp * pos
[@@deriving show]

and exp =
  | VarExp of var
  | NilExp
  | UnitExp
  | IntExp of int
  | StringExp of string * pos
  | CallExp of {func: symbol; args: exp list; pos: pos}
  | OpExp of {left: exp; oper: oper; right: exp; pos: pos}
  | RecordExp of {fields: (symbol * exp * pos) list;
		              typ: symbol; pos: pos}
  | SeqExp of (exp * pos) list
  | AssignExp of {var: var; exp: exp; pos: pos}
  | IfExp of {test: exp; then': exp; else': exp option; pos: pos}
  | WhileExp of {test: exp; body: exp; pos: pos}
	| ForExp of {var: symbol; escape: bool ref;
		            lo: exp; hi: exp; body: exp; pos: pos}
  | BreakExp of pos
  | LetExp of {decs: dec list; body: exp; pos: pos}
  | ArrayExp of {typ: symbol; size: exp; init: exp; pos: pos}
  | DummyExp
[@@deriving show]

and dec =
  | FunctionDec of fundec list
  | VarDec of {name: symbol;
		            escape: bool ref;
		            typ: (symbol * pos) option;
		            init: exp;
		            pos: pos}
  | TypeDec of typedec list
[@@deriving show]

and typedec = {name: symbol; ty: ty; pos: pos}
[@@deriving show]

and ty =
  | NameTy of symbol * pos
  | RecordTy of field list
  | ArrayTy of symbol * pos
[@@deriving show]

and oper = PlusOp | MinusOp | TimesOp | DivideOp
         | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
[@@deriving show]

and field = {name: symbol; escape: bool ref;
		          typ: symbol; pos: pos}
[@@deriving show]

and fundec = {name: symbol;
		          params: field list;
		          result: (symbol * pos) option;
		          body: exp;
		          pos: pos}
[@@deriving show]