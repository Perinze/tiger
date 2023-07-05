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

let rec equal (a : ty) (b : ty) : bool =
  match (a, b) with
  | (RECORD (a_fields, a_unique), RECORD (b_fields, b_unique)) ->
    if a_unique != b_unique then
      false
    else
      let field_equal (a : Symbol.symbol * ty) (b : Symbol.symbol * ty) : bool =
        if (fst a) <> (fst b) then
          false
        else if not (equal (snd a) (snd b)) then
          false
        else
          true in
      let rec fields_equal a b =
        match (a, b) with
        | ((a_head :: a_rest), (b_head :: b_rest)) ->
          (field_equal a_head b_head) && (fields_equal a_rest b_rest)
        | _ -> false in
      fields_equal a_fields b_fields

  | (ARRAY (a_ty, a_unique), ARRAY (b_ty, b_unique)) ->
    if a_unique != b_unique then
      false
    else
      equal a_ty b_ty

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