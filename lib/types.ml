type unique = unit ref

type ty =
| RECORD of (Symbol.symbol * ty) list * unique
| NIL
| INT
| STRING
| ARRAY of ty * unique
| NAME of Symbol.symbol * ty option ref (* place holder for recursion *)
| UNIT

let rec format = function
| RECORD (sym_ty_list, _) ->
  let fmt (sym, ty) = (Symbol.name sym) ^ " : " ^ (format ty) in
  "{" ^ (List.fold_left (fun acc a -> acc ^ "; " ^ (fmt a)) "" sym_ty_list) ^ "}"
| NIL -> "nil"
| INT -> "int"
| STRING -> "string"
| ARRAY (ty, _) -> "[" ^ (format ty) ^ "]"
| NAME _ -> "name"
| UNIT -> "unit"