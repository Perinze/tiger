type unique = unit ref [@@deriving show]

type ty =
| RECORD of (Symbol.symbol * ty) list * unique
| NIL
| INT
| STRING
| ARRAY of ty * unique
| NAME of Symbol.symbol * ty option ref (* place holder for recursion *)
| UNIT
[@@deriving show]

type tylist = ty list [@@deriving show]

let eq (a : ty) (b : ty) : bool =
  match (a, b) with
  | (RECORD (_, a_unique), RECORD (_, b_unique)) ->
    a_unique == b_unique
  | (NIL, RECORD _) ->
    true
  | (RECORD _, NIL) ->
    true

  | (ARRAY (_, a_unique), ARRAY (_, b_unique)) ->
    a_unique == b_unique 

  | (NAME (a_sym, a_ref), NAME (b_sym, b_ref)) ->
    if a_sym <> b_sym then
      false
    else if a_ref != b_ref then
      false
    else
      true
  
  | (NIL, NIL) -> true
  | (INT, INT) -> true
  | (STRING, STRING) -> true
  | (UNIT, UNIT) -> true
  | _ -> false

let neq (a : ty) (b : ty) : bool = not (eq a b)

(*
let rec format = function
| RECORD (sym_ty_list, _) ->
  let fmt (sym, ty) = (Symbol.name sym) ^ " : " ^ (format ty) in
  "{" ^ (List.fold_left (fun acc a -> acc ^ (fmt a)) "" sym_ty_list ^ "; ") ^ "}"
| NIL -> "nil"
| INT -> "int"
| STRING -> "string"
| ARRAY (ty, _) -> "[" ^ (format ty) ^ "]"
| NAME (a, _) -> "name#" ^ (Symbol.name a)
| UNIT -> "unit"
*)